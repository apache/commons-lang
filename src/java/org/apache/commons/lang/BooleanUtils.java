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

/**
 * <p><code>BooleanUtils</code> contains utility methods for working for
 * boolean and Boolean objects.</p>
 *
 * @author Stephen Colebourne
 * @since 2.0
 * @version $Id: BooleanUtils.java,v 1.4 2003/02/04 22:50:31 scolebourne Exp $
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
     * <p>Converts a Boolean to a boolean handling null by returning false.</p>
     * 
     * @param bool  the boolean to convert
     * @return true or false
     */
    public static boolean toBoolean(Boolean bool) {
        if (bool == null) {
            return false;
        }
        return (bool.booleanValue() ? true : false);
    }
    
    /**
     * <p>Converts a Boolean to a boolean handling null.</p>
     * 
     * @param bool  the boolean to convert
     * @param valueIfNull  the boolean value to return if null
     * @return true or false
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
     * <p>Convert an int to a boolean using the convention that zero is false.</p>
     * 
     * @param value  the int to convert
     * @return true if non-zero, false if zero
     */
    public static boolean toBoolean(int value) {
        return (value == 0 ? false : true);
    }
    
    /**
     * <p>Convert an int to a Boolean using the convention that zero is false.</p>
     * 
     * @param value  the int to convert
     * @return Boolean.TRUE if non-zero, Boolean.FALSE if zero, null if null
     */
    public static Boolean toBooleanObject(int value) {
        return (value == 0 ? Boolean.FALSE : Boolean.TRUE);
    }
    
    /**
     * <p>Convert an Integer to a Boolean using the convention that zero is false.</p>
     * 
     * <p>null will be converted to null.</p>
     * 
     * @param value  the Integer to convert
     * @return Boolean.TRUE if non-zero, Boolean.FALSE if zero, null if null
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
     * @param trueValue  the value to match for true
     * @param falseValue  the value to match for false
     * @return true or false
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
     * @param trueValue  the value to match for true, may be null
     * @param falseValue  the value to match for false, may be null
     * @return true or false
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
     * @param trueValue  the value to match for true
     * @param falseValue  the value to match for false
     * @param nullValue  the value to to match for null
     * @return Boolean.TRUE, Boolean.FALSE, or null
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
     * @param trueValue  the value to match for true, may be null
     * @param falseValue  the value to match for false, may be null
     * @param nullValue  the value to to match for null, may be null
     * @return Boolean.TRUE, Boolean.FALSE, or null
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
     * <p>Convert a boolean to an int using the convention that zero is false.</p>
     * 
     * @param bool  the boolean to convert
     * @return one if true, zero if false
     */
    public static int toInteger(boolean bool) {
        return (bool ? 1 : 0);
    }
    
    /**
     * <p>Convert a boolean to an Integer using the convention that zero is false.</p>
     * 
     * @param bool  the boolean to convert
     * @return one if true, zero if false
     */
    public static Integer toIntegerObject(boolean bool) {
        return (bool ? NumberUtils.INTEGER_ONE : NumberUtils.INTEGER_ZERO);
    }
    
    /**
     * <p>Convert a Boolean to a Integer using the convention that zero is false.</p>
     * 
     * <p>null will be converted to null.</p>
     * 
     * @param bool  the Boolean to convert
     * @return one if Boolean.TRUE, zero if Boolean.FALSE, null if null
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
     * @param trueValue  the value to return if true
     * @param falseValue  the value to return if false
     * @return the appropriate value
     */
    public static int toInteger(boolean bool, int trueValue, int falseValue) {
        return (bool ? trueValue : falseValue);
    }
    
    /**
     * <p>Convert a Boolean to an int specifying the conversion values.</p>
     * 
     * @param bool  the Boolean to convert
     * @param trueValue  the value to return if true
     * @param falseValue  the value to return if false
     * @param nullValue  the value to return if null
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
     * @param trueValue  the value to return if true, may be null
     * @param falseValue  the value to return if false, may be null
     * @return the appropriate value
     */
    public static Integer toIntegerObject(boolean bool, Integer trueValue, Integer falseValue) {
        return (bool ? trueValue : falseValue);
    }
    
    /**
     * <p>Convert a Boolean to an Integer specifying the conversion values.</p>
     * 
     * @param bool  the Boolean to convert
     * @param trueValue  the value to return if true, may be null
     * @param falseValue  the value to return if false, may be null
     * @param nullValue  the value to return if null, may be null
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
     * <p>'true', 'on' or 'yes' (case insensitive) will return true.
     * 'false', 'off' or 'no' (case insensitive) will return false.
     * Otherwise, null is returned.</p>
     *
     * @param str  the String to check
     * @return the Boolean value of the string, null if no match or null input
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
     * @param trueString  the String to match for true (case sensitive), may be null
     * @param falseString  the String to match for false (case sensitive), may be null
     * @param nullString  the String to match for null (case sensitive), may be null
     * @return the Boolean value of the string, null if no match or null input
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
     * <p>'true', 'on' or 'yes' (case insensitive) will return true.
     * Otherwise, false is returned.</p>
     *
     * @param str  the String to check
     * @return the boolean value of the string, false if no match
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
     * @param trueString  the String to match for true (case sensitive), may be null
     * @param falseString  the String to match for false (case sensitive), may be null
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
     * <p>Converts a Boolean to a String returning 'true', 'false', or <code>null</code>.</p>
     * 
     * @param bool  the Boolean to check
     * @return 'true', 'false', or <code>null</code>
     */
    public static String toStringTrueFalse(Boolean bool) {
        return toString(bool, "true", "false", null);
    }
    
    /**
     * <p>Converts a Boolean to a String returning 'on', 'off', or <code>null</code>.</p>
     * 
     * @param bool  the Boolean to check
     * @return 'on', 'off', or <code>null</code>
     */
    public static String toStringOnOff(Boolean bool) {
        return toString(bool, "on", "off", null);
    }
    
    /**
     * <p>Converts a Boolean to a String returning 'yes', 'no', or <code>null</code>.</p>
     * 
     * @param bool  the Boolean to check
     * @return 'yes', 'no', or <code>null</code>
     */
    public static String toStringYesNo(Boolean bool) {
        return toString(bool, "yes", "no", null);
    }
    
    /**
     * <p>Converts a Boolean to a String returning one of the input Strings.</p>
     * 
     * @param bool  the Boolean to check
     * @param trueString  the String to return if true, may be null
     * @param falseString  the String to return if false, may be null
     * @param nullString  the String to return if null, may be null
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
     * <p>Converts a boolean to a String returning 'true' or 'false'.</p>
     * 
     * @param bool  the Boolean to check
     * @return 'true', 'false', or <code>null</code>
     */
    public static String toStringTrueFalse(boolean bool) {
        return toString(bool, "true", "false");
    }
    
    /**
     * <p>Converts a boolean to a String returning 'on' or 'off'.</p>
     * 
     * @param bool  the Boolean to check
     * @return 'on', 'off', or <code>null</code>
     */
    public static String toStringOnOff(boolean bool) {
        return toString(bool, "on", "off");
    }
    
    /**
     * <p>Converts a boolean to a String returning 'yes' or 'no'.</p>
     * 
     * @param bool  the Boolean to check
     * @return 'yes', 'no', or <code>null</code>
     */
    public static String toStringYesNo(boolean bool) {
        return toString(bool, "yes", "no");
    }
    
    /**
     * <p>Converts a boolean to a String returning one of the input Strings.</p>
     * 
     * @param bool  the Boolean to check
     * @param trueString  the String to return if true, may be null
     * @param falseString  the String to return if false, may be null
     * @return one of the two input Strings
     */
    public static String toString(boolean bool, String trueString, String falseString) {
        return (bool ? trueString : falseString);
    }
    
}
