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
package org.apache.commons.lang;

import org.apache.commons.lang.math.NumberUtils;

/**
 * <p><code>BooleanUtils</code> contains utility methods for working for
 * boolean and Boolean objects.</p>
 *
 * @author Stephen Colebourne
 * @author Matthew Hawthorne
 * @since 2.0
 * @version $Id: BooleanUtils.java,v 1.7 2003/07/14 22:25:02 bayard Exp $
 */
public class BooleanUtils {

    /**
     * <p><code>BooleanUtils</code> instances should NOT be constructed in standard programming.
     * Instead, the class should be used as <code>BooleanUtils.toBooleanObject(true);</code>.</p>
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public BooleanUtils() {
    }

    // Boolean utilities
    //--------------------------------------------------------------------------
    /**
     * <p>Negates the specified boolean.</p>
     * 
     * <p>If <code>null</code> is passed in, <code>null</code> will be returned.</p>
     * 
     * @param bool  the Boolean to negate, may be null
     * @return the negated Boolean, or <code>null</code> if <code>null</code> passed in
     */
    public static Boolean negate(Boolean bool) {
        if (bool == null) {
            return null;
        }
        return (bool.booleanValue() ? Boolean.FALSE : Boolean.TRUE);
    }
    
    // boolean Boolean methods
    //--------------------------------------------------------------------------
    /**
     * <p>Boolean factory that avoids creating new Boolean objecs all the time.</p>
     * 
     * <p>This method was added to JDK1.4 but is available here for earlier JDKs.</p>
     * 
     * @param bool  the boolean to convert
     * @return Boolean.TRUE or Boolean.FALSE as appropriate
     */
    public static Boolean toBooleanObject(boolean bool) {
        return (bool ? Boolean.TRUE : Boolean.FALSE);
    }
    
    /**
     * <p>Converts a Boolean to a boolean handling <code>null</code>
     * by returning <code>false</code>.</p>
     * 
     * @param bool  the boolean to convert
     * @return <code>true</code> or <code>false</code>
     */
    public static boolean toBoolean(Boolean bool) {
        if (bool == null) {
            return false;
        }
        return (bool.booleanValue() ? true : false);
    }
    
    /**
     * <p>Converts a Boolean to a boolean handling <code>null</code>.</p>
     * 
     * @param bool  the boolean to convert
     * @param valueIfNull  the boolean value to return if <code>null</code>
     * @return <code>true</code> or <code>false</code>
     */
    public static boolean toBooleanDefaultIfNull(Boolean bool, boolean valueIfNull) {
        if (bool == null) {
            return valueIfNull;
        }
        return (bool.booleanValue() ? true : false);
    }
    
    // Integer to Boolean methods
    //--------------------------------------------------------------------------
    /**
     * <p>Convert an int to a boolean using the convention that <code>zero</code>
     * is <code>false</code>.</p>
     * 
     * @param value  the int to convert
     * @return <code>true</code> if non-zero, <code>false</code>
     *  if zero
     */
    public static boolean toBoolean(int value) {
        return (value == 0 ? false : true);
    }
    
    /**
     * <p>Convert an int to a Boolean using the convention that <code>zero</code>
     * is <code>false</code>.</p>
     * 
     * @param value  the int to convert
     * @return Boolean.TRUE if non-zero, Boolean.FALSE if zero,
     *  <code>null</code> if <code>null</code>
     */
    public static Boolean toBooleanObject(int value) {
        return (value == 0 ? Boolean.FALSE : Boolean.TRUE);
    }
    
    /**
     * <p>Convert an Integer to a Boolean using the convention that <code>zero</code>
     * is <code>false</code>.</p>
     * 
     * <p><code>null</code> will be converted to <code>null</code>.</p>
     * 
     * @param value  the Integer to convert
     * @return Boolean.TRUE if non-zero, Boolean.FALSE if zero,
     *  <code>null</code> if <code>null</code>
     */
    public static Boolean toBooleanObject(Integer value) {
        if (value == null) {
            return null;
        }
        return (value.intValue() == 0 ? Boolean.FALSE : Boolean.TRUE);
    }
    
    /**
     * <p>Convert an int to a boolean specifying the conversion values.</p>
     * 
     * @param value  the Integer to convert
     * @param trueValue  the value to match for <code>true</code>
     * @param falseValue  the value to match for <code>false</code>
     * @return <code>true</code> or <code>false</code>
     * @throws IllegalArgumentException if no match
     */
    public static boolean toBoolean(int value, int trueValue, int falseValue) {
        if (value == trueValue) {
            return true;
        } else if (value == falseValue) {
            return false;
        }
        // no match
        throw new IllegalArgumentException("The Integer did not match either specified value");
    }
    
    /**
     * <p>Convert an Integer to a boolean specifying the conversion values.</p>
     * 
     * @param value  the Integer to convert
     * @param trueValue  the value to match for <code>true</code>,
     *  may be <code>null</code>
     * @param falseValue  the value to match for <code>false</code>,
     *  may be <code>null</code>
     * @return <code>true</code> or <code>false</code>
     * @throws IllegalArgumentException if no match
     */
    public static boolean toBoolean(Integer value, Integer trueValue, Integer falseValue) {
        if (value == null) {
            if (trueValue == null) {
                return true;
            } else if (falseValue == null) {
                return false;
            }
        } else if (value.equals(trueValue)) {
            return true;
        } else if (value.equals(falseValue)) {
            return false;
        }
        // no match
        throw new IllegalArgumentException("The Integer did not match either specified value");
    }
    
    /**
     * <p>Convert an int to a Boolean specifying the conversion values.</p>
     * 
     * @param value  the Integer to convert
     * @param trueValue  the value to match for <code>true</code>
     * @param falseValue  the value to match for <code>false</code>
     * @param nullValue  the value to to match for <code>null</code>
     * @return Boolean.TRUE, Boolean.FALSE, or <code>null</code>
     * @throws IllegalArgumentException if no match
     */
    public static Boolean toBooleanObject(int value, int trueValue, int falseValue, int nullValue) {
        if (value == trueValue) {
            return Boolean.TRUE;
        } else if (value == falseValue) {
            return Boolean.FALSE;
        } else if (value == nullValue) {
            return null;
        }
        // no match
        throw new IllegalArgumentException("The Integer did not match any specified value");
    }
    
    /**
     * <p>Convert an Integer to a Boolean specifying the conversion values.</p>
     * 
     * @param value  the Integer to convert
     * @param trueValue  the value to match for <code>true</code>,
     *  may be <code>null</code>
     * @param falseValue  the value to match for <code>false</code>,
     *  may be <code>null</code>
     * @param nullValue  the value to to match for <code>null</code>,
     *  may be <code>null</code>
     * @return Boolean.TRUE, Boolean.FALSE, or <code>null</code>
     * @throws IllegalArgumentException if no match
     */
    public static Boolean toBooleanObject(Integer value, Integer trueValue, Integer falseValue, Integer nullValue) {
        if (value == null) {
            if (trueValue == null) {
                return Boolean.TRUE;
            } else if (falseValue == null) {
                return Boolean.FALSE;
            } else if (nullValue == null) {
                return null;
            }
        } else if (value.equals(trueValue)) {
            return Boolean.TRUE;
        } else if (value.equals(falseValue)) {
            return Boolean.FALSE;
        } else if (value.equals(nullValue)) {
            return null;
        }
        // no match
        throw new IllegalArgumentException("The Integer did not match any specified value");
    }
    
    // Boolean to Integer methods
    //--------------------------------------------------------------------------
    /**
     * <p>Convert a boolean to an int using the convention that
     * <code>zero</code> is <code>false</code>.</p>
     * 
     * @param bool  the boolean to convert
     * @return one if <code>true</code>, zero if <code>false</code>
     */
    public static int toInteger(boolean bool) {
        return (bool ? 1 : 0);
    }
    
    /**
     * <p>Convert a boolean to an Integer using the convention that
     * <code>zero</code> is <code>false</code>.</p>
     * 
     * @param bool  the boolean to convert
     * @return one if <code>true</code>, zero if <code>false</code>
     */
    public static Integer toIntegerObject(boolean bool) {
        return (bool ? NumberUtils.INTEGER_ONE : NumberUtils.INTEGER_ZERO);
    }
    
    /**
     * <p>Convert a Boolean to a Integer using the convention that
     * <code>zero</code> is <code>false</code>.</p>
     * 
     * <p><code>null</code> will be converted to <code>null</code>.</p>
     * 
     * @param bool  the Boolean to convert
     * @return one if Boolean.TRUE, zero if Boolean.FALSE, <code>null</code> if <code>null</code>
     */
    public static Integer toIntegerObject(Boolean bool) {
        if (bool == null) {
            return null;
        }
        return (bool.booleanValue() ? NumberUtils.INTEGER_ONE : NumberUtils.INTEGER_ZERO);
    }
    
    /**
     * <p>Convert a boolean to an int specifying the conversion values.</p>
     * 
     * @param bool  the to convert
     * @param trueValue  the value to return if <code>true</code>
     * @param falseValue  the value to return if <code>false</code>
     * @return the appropriate value
     */
    public static int toInteger(boolean bool, int trueValue, int falseValue) {
        return (bool ? trueValue : falseValue);
    }
    
    /**
     * <p>Convert a Boolean to an int specifying the conversion values.</p>
     * 
     * @param bool  the Boolean to convert
     * @param trueValue  the value to return if <code>true</code>
     * @param falseValue  the value to return if <code>false</code>
     * @param nullValue  the value to return if <code>null</code>
     * @return the appropriate value
     */
    public static int toInteger(Boolean bool, int trueValue, int falseValue, int nullValue) {
        if (bool == null) {
            return nullValue;
        }
        return (bool.booleanValue() ? trueValue : falseValue);
    }
    
    /**
     * <p>Convert a boolean to an Integer specifying the conversion values.</p>
     * 
     * @param bool  the to convert
     * @param trueValue  the value to return if <code>true</code>,
     *  may be <code>null</code>
     * @param falseValue  the value to return if <code>false</code>,
     *  may be <code>null</code>
     * @return the appropriate value
     */
    public static Integer toIntegerObject(boolean bool, Integer trueValue, Integer falseValue) {
        return (bool ? trueValue : falseValue);
    }
    
    /**
     * <p>Convert a Boolean to an Integer specifying the conversion values.</p>
     * 
     * @param bool  the Boolean to convert
     * @param trueValue  the value to return if <code>true</code>,
     *  may be <code>null</code>
     * @param falseValue  the value to return if <code>false</code>,
     *  may be <code>null</code>
     * @param nullValue  the value to return if <code>null</code>,
     *  may be <code>null</code>
     * @return the appropriate value
     */
    public static Integer toIntegerObject(Boolean bool, Integer trueValue, Integer falseValue, Integer nullValue) {
        if (bool == null) {
            return nullValue;
        }
        return (bool.booleanValue() ? trueValue : falseValue);
    }
    
    // String to Boolean methods
    //--------------------------------------------------------------------------
    /**
     * <p>Converts a String to a Boolean.</p>
     * 
     * <p><code>'true'</code>, <code>'on'</code> or <code>'yes'</code>
     * (case insensitive) will return <code>true</code>.
     * <code>'false'</code>, <code>'off'</code> or <code>'no'</code>
     * (case insensitive) will return <code>false</code>.
     * Otherwise, <code>null</code> is returned.</p>
     *
     * @param str  the String to check
     * @return the Boolean value of the string, <code>null</code>
     *  if no match or <code>null</code> input
     */
    public static Boolean toBooleanObject(String str) {
        if ("true".equalsIgnoreCase(str)) {
            return Boolean.TRUE;
        } else if ("false".equalsIgnoreCase(str)) {
            return Boolean.FALSE;
        } else if ("on".equalsIgnoreCase(str)) {
            return Boolean.TRUE;
        } else if ("off".equalsIgnoreCase(str)) {
            return Boolean.FALSE;
        } else if ("yes".equalsIgnoreCase(str)) {
            return Boolean.TRUE;
        } else if ("no".equalsIgnoreCase(str)) {
            return Boolean.FALSE;
        }
        // no match
        return null;
    }

    /**
     * <p>Converts a String to a Boolean throwing an exception if no match.</p>
     *
     * @param str  the String to check
     * @param trueString  the String to match for <code>true</code>
     *  (case sensitive), may be <code>null</code>
     * @param falseString  the String to match for <code>false</code>
     *  (case sensitive), may be <code>null</code>
     * @param nullString  the String to match for <code>null</code>
     *  (case sensitive), may be <code>null</code>
     * @return the Boolean value of the string, <code>null</code>
     *  if no match or <code>null</code> input
     */
    public static Boolean toBooleanObject(String str, String trueString, String falseString, String nullString) {
        if (str == null) {
            if (trueString == null) {
                return Boolean.TRUE;
            } else if (falseString == null) {
                return Boolean.FALSE;
            } else if (nullString == null) {
                return null;
            }
        } else if (str.equals(trueString)) {
            return Boolean.TRUE;
        } else if (str.equals(falseString)) {
            return Boolean.FALSE;
        } else if (str.equals(nullString)) {
            return null;
        }
        // no match
        throw new IllegalArgumentException("The String did not match any specified value");
    }

    // String to boolean methods
    //--------------------------------------------------------------------------
    /**
     * <p>Converts a String to a boolean.</p>
     * 
     * <p><code>'true'</code>, <code>'on'</code> or <code>'yes'</code>
     * (case insensitive) will return <code>true</code>. Otherwise,
     * <code>false</code> is returned.</p>
     *
     * @param str  the String to check
     * @return the boolean value of the string, <code>false</code> if no match
     */
    public static boolean toBoolean(String str) {
        if ("true".equalsIgnoreCase(str)) {
            return true;
        } else if ("on".equalsIgnoreCase(str)) {
            return true;
        } else if ("yes".equalsIgnoreCase(str)) {
            return true;
        }
        // no match
        return false;
    }

    /**
     * <p>Converts a String to a Boolean throwing an exception if no match found.</p>
     * 
     * <p>null is returned if there is no match.</p>
     *
     * @param str  the String to check
     * @param trueString  the String to match for <code>true</code>
     *  (case sensitive), may be <code>null</code>
     * @param falseString  the String to match for <code>false</code>
     *  (case sensitive), may be <code>null</code>
     * @return the boolean value of the string
     * @throws IllegalArgumentException if the String doesn't match
     */
    public static boolean toBoolean(String str, String trueString, String falseString) {
        if (str == null) {
            if (trueString == null) {
                return true;
            } else if (falseString == null) {
                return false;
            }
        } else if (str.equals(trueString)) {
            return true;
        } else if (str.equals(falseString)) {
            return false;
        }
        // no match
        throw new IllegalArgumentException("The String did not match either specified value");
    }

    // Boolean to String methods
    //--------------------------------------------------------------------------
    /**
     * <p>Converts a Boolean to a String returning <code>'true'</code>,
     * <code>'false'</code>, or <code>null</code>.</p>
     * 
     * @param bool  the Boolean to check
     * @return <code>'true'</code>, <code>'false'</code>,
     *  or <code>null</code>
     */
    public static String toStringTrueFalse(Boolean bool) {
        return toString(bool, "true", "false", null);
    }
    
    /**
     * <p>Converts a Boolean to a String returning <code>'on'</code>,
     * <code>'off'</code>, or <code>null</code>.</p>
     * 
     * @param bool  the Boolean to check
     * @return <code>'on'</code>, <code>'off'</code>,
     *  or <code>null</code>
     */
    public static String toStringOnOff(Boolean bool) {
        return toString(bool, "on", "off", null);
    }
    
    /**
     * <p>Converts a Boolean to a String returning <code>'yes'</code>,
     * <code>'no'</code>, or <code>null</code>.</p>
     * 
     * @param bool  the Boolean to check
     * @return <code>'yes'</code>, <code>'no'</code>,
     *  or <code>null</code>
     */
    public static String toStringYesNo(Boolean bool) {
        return toString(bool, "yes", "no", null);
    }
    
    /**
     * <p>Converts a Boolean to a String returning one of the input Strings.</p>
     * 
     * @param bool  the Boolean to check
     * @param trueString  the String to return if <code>true</code>,
     *  may be <code>null</code>
     * @param falseString  the String to return if <code>false</code>,
     *  may be <code>null</code>
     * @param nullString  the String to return if <code>null</code>,
     *  may be <code>null</code>
     * @return one of the three input Strings
     */
    public static String toString(Boolean bool, String trueString, String falseString, String nullString) {
        if (bool == null) {
            return nullString;
        }
        return (bool.booleanValue() ? trueString : falseString);
    }
    
    // boolean to String methods
    //--------------------------------------------------------------------------
    /**
     * <p>Converts a boolean to a String returning <code>'true'</code>
     * or <code>'false'</code>.</p>
     * 
     * @param bool  the Boolean to check
     * @return <code>'true'</code>, <code>'false'</code>,
     *  or <code>null</code>
     */
    public static String toStringTrueFalse(boolean bool) {
        return toString(bool, "true", "false");
    }
    
    /**
     * <p>Converts a boolean to a String returning <code>'on'</code>
     * or <code>'off'</code>.</p>
     * 
     * @param bool  the Boolean to check
     * @return <code>'on'</code>, <code>'off'</code>,
     *  or <code>null</code>
     */
    public static String toStringOnOff(boolean bool) {
        return toString(bool, "on", "off");
    }
    
    /**
     * <p>Converts a boolean to a String returning <code>'yes'</code>
     * or <code>'no'</code>.</p>
     * 
     * @param bool  the Boolean to check
     * @return <code>'yes'</code>, <code>'no'</code>,
     *  or <code>null</code>
     */
    public static String toStringYesNo(boolean bool) {
        return toString(bool, "yes", "no");
    }
    
    /**
     * <p>Converts a boolean to a String returning one of the input Strings.</p>
     * 
     * @param bool  the Boolean to check
     * @param trueString  the String to return if <code>true</code>,
     *  may be <code>null</code>
     * @param falseString  the String to return if <code>false</code>,
     *  may be <code>null</code>
     * @return one of the two input Strings
     */
    public static String toString(boolean bool, String trueString, String falseString) {
        return (bool ? trueString : falseString);
    }
    
    // xor methods
    //  --------------------------------------------------------------------------
    /**
     * <p>Performs an xor on a set of booleans.</p>
     * 
     * @param array  an array of <code>boolean<code>s
     * @return <code>true</code> if the xor is successful.
     * @throws NullArgumentException if <code>array</code> is <code>null</code>
     * @throws IllegalArgumentException if <code>array</code> is empty.
     */
    public static boolean xor(boolean[] array) {
        // Validates input
        if (array == null) {
            throw new NullArgumentException("Array");
        } else if (array.length == 0) {
            throw new IllegalArgumentException("Array is empty");
        }

        // Loops through array, comparing each item
        int trueCount = 0;
        for (int i = 0; i < array.length; i++) {
            // If item is true, and trueCount is < 1, increments count
            // Else, xor fails
            if (array[i]) {
                if (trueCount < 1) {
                    trueCount++;
                } else {
                    return false;
                }
            }
        }

        // Returns true if there was exactly 1 true item
        return trueCount == 1;
    }

    /**
     * <p>Performs an xor on an array of Booleans.</p>
     * 
     * @param array  an array of <code>Boolean<code>s
     * @return <code>true</code> if the xor is successful.
     * @throws NullPointerException if <code>array</code> contains a <code>null</code>
     * @throws NullArgumentException if <code>array</code> is <code>null</code>
     * @throws IllegalArgumentException if <code>array</code> is empty.
     */
    public static Boolean xor(Boolean[] array) {
        return new Boolean(xor(ArrayUtils.toPrimitive(array)));
    }

}
