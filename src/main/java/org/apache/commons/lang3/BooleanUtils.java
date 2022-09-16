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
package org.apache.commons.lang3;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;

import org.apache.commons.lang3.math.NumberUtils;

/**
 * Operations on boolean primitives and Boolean objects.
 *
 * <p>This class tries to handle {@code null} input gracefully.
 * An exception will not be thrown for a {@code null} input.
 * Each method documents its behavior in more detail.</p>
 *
 * <p>#ThreadSafe#</p>
 * @since 2.0
 */
public class BooleanUtils {

    private static final List<Boolean> BOOLEAN_LIST = Collections.unmodifiableList(Arrays.asList(Boolean.FALSE, Boolean.TRUE));

    /**
     * The false String {@code "false"}.
     *
     * @since 3.12.0
     */
    public static final String FALSE = "false";

    /**
     * The no String {@code "no"}.
     *
     * @since 3.12.0
     */
    public static final String NO = "no";

    /**
     * The off String {@code "off"}.
     *
     * @since 3.12.0
     */
    public static final String OFF = "off";

    /**
     * The on String {@code "on"}.
     *
     * @since 3.12.0
     */
    public static final String ON = "on";

    /**
     * The true String {@code "true"}.
     *
     * @since 3.12.0
     */
    public static final String TRUE = "true";

    /**
     * The yes String {@code "yes"}.
     *
     * @since 3.12.0
     */
    public static final String YES = "yes";

    /**
     * Performs an 'and' operation on a set of booleans.
     *
     * <pre>
     *   BooleanUtils.and(true, true)         = true
     *   BooleanUtils.and(false, false)       = false
     *   BooleanUtils.and(true, false)        = false
     *   BooleanUtils.and(true, true, false)  = false
     *   BooleanUtils.and(true, true, true)   = true
     * </pre>
     *
     * @param array  an array of {@code boolean}s
     * @return the result of the logical 'and' operation. That is {@code false}
     * if any of the parameters is {@code false} and {@code true} otherwise.
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty.
     * @since 3.0.1
     */
    public static boolean and(final boolean... array) {
        ObjectUtils.requireNonEmpty(array, "array");
        for (final boolean element : array) {
            if (!element) {
                return false;
            }
        }
        return true;
    }

    /**
     * Performs an 'and' operation on an array of Booleans.
     * <pre>
     *   BooleanUtils.and(Boolean.TRUE, Boolean.TRUE)                 = Boolean.TRUE
     *   BooleanUtils.and(Boolean.FALSE, Boolean.FALSE)               = Boolean.FALSE
     *   BooleanUtils.and(Boolean.TRUE, Boolean.FALSE)                = Boolean.FALSE
     *   BooleanUtils.and(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE)   = Boolean.TRUE
     *   BooleanUtils.and(Boolean.FALSE, Boolean.FALSE, Boolean.TRUE) = Boolean.FALSE
     *   BooleanUtils.and(Boolean.TRUE, Boolean.FALSE, Boolean.TRUE)  = Boolean.FALSE
     *   BooleanUtils.and(null, null)                                 = Boolean.FALSE
     * </pre>
     * <p>
     * Null array elements map to false, like {@code Boolean.parseBoolean(null)} and its callers return false.
     * </p>
     *
     * @param array  an array of {@link Boolean}s
     * @return the result of the logical 'and' operation. That is {@code false}
     * if any of the parameters is {@code false} and {@code true} otherwise.
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty.
     * @since 3.0.1
     */
    public static Boolean and(final Boolean... array) {
        ObjectUtils.requireNonEmpty(array, "array");
        return and(ArrayUtils.toPrimitive(array)) ? Boolean.TRUE : Boolean.FALSE;
    }

    /**
     * Returns a new array of possible values (like an enum would).
     * @return a new array of possible values (like an enum would).
     * @since 3.12.0
     */
    public static Boolean[] booleanValues() {
        return new Boolean[] {Boolean.FALSE, Boolean.TRUE};
    }

    /**
     * Compares two {@code boolean} values. This is the same functionality as provided in Java 7.
     *
     * @param x the first {@code boolean} to compare
     * @param y the second {@code boolean} to compare
     * @return the value {@code 0} if {@code x == y};
     *         a value less than {@code 0} if {@code !x && y}; and
     *         a value greater than {@code 0} if {@code x && !y}
     * @since 3.4
     */
    public static int compare(final boolean x, final boolean y) {
        if (x == y) {
            return 0;
        }
        return x ? 1 : -1;
    }

    /**
     * Performs the given action for each Boolean {@link BooleanUtils#values()}.
     *
     * @param action The action to be performed for each element
     * @since 3.13.0
     */
    public static void forEach(final Consumer<Boolean> action) {
        values().forEach(action);
    }

    /**
     * Checks if a {@link Boolean} value is {@code false},
     * handling {@code null} by returning {@code false}.
     *
     * <pre>
     *   BooleanUtils.isFalse(Boolean.TRUE)  = false
     *   BooleanUtils.isFalse(Boolean.FALSE) = true
     *   BooleanUtils.isFalse(null)          = false
     * </pre>
     *
     * @param bool  the boolean to check, null returns {@code false}
     * @return {@code true} only if the input is non-{@code null} and {@code false}
     * @since 2.1
     */
    public static boolean isFalse(final Boolean bool) {
        return Boolean.FALSE.equals(bool);
    }

    /**
     * Checks if a {@link Boolean} value is <i>not</i> {@code false},
     * handling {@code null} by returning {@code true}.
     *
     * <pre>
     *   BooleanUtils.isNotFalse(Boolean.TRUE)  = true
     *   BooleanUtils.isNotFalse(Boolean.FALSE) = false
     *   BooleanUtils.isNotFalse(null)          = true
     * </pre>
     *
     * @param bool  the boolean to check, null returns {@code true}
     * @return {@code true} if the input is {@code null} or {@code true}
     * @since 2.3
     */
    public static boolean isNotFalse(final Boolean bool) {
        return !isFalse(bool);
    }

    /**
     * Checks if a {@link Boolean} value is <i>not</i> {@code true},
     * handling {@code null} by returning {@code true}.
     *
     * <pre>
     *   BooleanUtils.isNotTrue(Boolean.TRUE)  = false
     *   BooleanUtils.isNotTrue(Boolean.FALSE) = true
     *   BooleanUtils.isNotTrue(null)          = true
     * </pre>
     *
     * @param bool  the boolean to check, null returns {@code true}
     * @return {@code true} if the input is null or false
     * @since 2.3
     */
    public static boolean isNotTrue(final Boolean bool) {
        return !isTrue(bool);
    }

    /**
     * Checks if a {@link Boolean} value is {@code true},
     * handling {@code null} by returning {@code false}.
     *
     * <pre>
     *   BooleanUtils.isTrue(Boolean.TRUE)  = true
     *   BooleanUtils.isTrue(Boolean.FALSE) = false
     *   BooleanUtils.isTrue(null)          = false
     * </pre>
     *
     * @param bool the boolean to check, {@code null} returns {@code false}
     * @return {@code true} only if the input is non-null and true
     * @since 2.1
     */
    public static boolean isTrue(final Boolean bool) {
        return Boolean.TRUE.equals(bool);
    }
    /**
     * Negates the specified boolean.
     *
     * <p>If {@code null} is passed in, {@code null} will be returned.</p>
     *
     * <p>NOTE: This returns {@code null} and will throw a {@link NullPointerException}
     * if unboxed to a boolean.</p>
     *
     * <pre>
     *   BooleanUtils.negate(Boolean.TRUE)  = Boolean.FALSE;
     *   BooleanUtils.negate(Boolean.FALSE) = Boolean.TRUE;
     *   BooleanUtils.negate(null)          = null;
     * </pre>
     *
     * @param bool  the Boolean to negate, may be null
     * @return the negated Boolean, or {@code null} if {@code null} input
     */
    public static Boolean negate(final Boolean bool) {
        if (bool == null) {
            return null;
        }
        return bool.booleanValue() ? Boolean.FALSE : Boolean.TRUE;
    }

    /**
     * Performs a one-hot on an array of booleans.
     * <p>
     * This implementation returns true if one, and only one, of the supplied values is true.
     * </p>
     * <p>
     * See also <a href="https://en.wikipedia.org/wiki/One-hot">One-hot</a>.
     * </p>
     * @param array  an array of {@code boolean}s
     * @return the result of the one-hot operations
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty.
     */
    public static boolean oneHot(final boolean... array) {
        ObjectUtils.requireNonEmpty(array, "array");
        boolean result = false;
        for (final boolean element: array) {
            if (element) {
                if (result) {
                    return false;
                }
                result = true;
            }
        }
        return result;
    }

    /**
     * Performs a one-hot on an array of booleans.
     * <p>
     * This implementation returns true if one, and only one, of the supplied values is true.
     * </p>
     * <p>
     * Null array elements map to false, like {@code Boolean.parseBoolean(null)} and its callers return false.
     * </p>
     * <p>
     * See also <a href="https://en.wikipedia.org/wiki/One-hot">One-hot</a>.
     * </p>
     *
     * @param array  an array of {@code boolean}s
     * @return the result of the one-hot operations
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty.
     */
    public static Boolean oneHot(final Boolean... array) {
        return Boolean.valueOf(oneHot(ArrayUtils.toPrimitive(array)));
    }

    /**
     * Performs an 'or' operation on a set of booleans.
     *
     * <pre>
     *   BooleanUtils.or(true, true)          = true
     *   BooleanUtils.or(false, false)        = false
     *   BooleanUtils.or(true, false)         = true
     *   BooleanUtils.or(true, true, false)   = true
     *   BooleanUtils.or(true, true, true)    = true
     *   BooleanUtils.or(false, false, false) = false
     * </pre>
     *
     * @param array  an array of {@code boolean}s
     * @return {@code true} if any of the arguments is {@code true}, and it returns {@code false} otherwise.
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty.
     * @since 3.0.1
     */
    public static boolean or(final boolean... array) {
        ObjectUtils.requireNonEmpty(array, "array");
        for (final boolean element : array) {
            if (element) {
                return true;
            }
        }
        return false;
    }

    /**
     * Performs an 'or' operation on an array of Booleans.
     * <pre>
     *   BooleanUtils.or(Boolean.TRUE, Boolean.TRUE)                  = Boolean.TRUE
     *   BooleanUtils.or(Boolean.FALSE, Boolean.FALSE)                = Boolean.FALSE
     *   BooleanUtils.or(Boolean.TRUE, Boolean.FALSE)                 = Boolean.TRUE
     *   BooleanUtils.or(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE)    = Boolean.TRUE
     *   BooleanUtils.or(Boolean.FALSE, Boolean.FALSE, Boolean.TRUE)  = Boolean.TRUE
     *   BooleanUtils.or(Boolean.TRUE, Boolean.FALSE, Boolean.TRUE)   = Boolean.TRUE
     *   BooleanUtils.or(Boolean.FALSE, Boolean.FALSE, Boolean.FALSE) = Boolean.FALSE
     *   BooleanUtils.or(Boolean.TRUE, null)                          = Boolean.TRUE
     *   BooleanUtils.or(Boolean.FALSE, null)                         = Boolean.FALSE
     * </pre>
     * <p>
     * Null array elements map to false, like {@code Boolean.parseBoolean(null)} and its callers return false.
     * </p>
     *
     * @param array  an array of {@link Boolean}s
     * @return {@code true} if any of the arguments is {@code true}, and it returns {@code false} otherwise.
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty.
     * @since 3.0.1
     */
    public static Boolean or(final Boolean... array) {
        ObjectUtils.requireNonEmpty(array, "array");
        return or(ArrayUtils.toPrimitive(array)) ? Boolean.TRUE : Boolean.FALSE;
    }

    /**
     * Returns a new array of possible values (like an enum would).
     * @return a new array of possible values (like an enum would).
     * @since 3.12.0
     */
    public static boolean[] primitiveValues() {
        return new boolean[] {false, true};
    }

    /**
     * Converts a Boolean to a boolean handling {@code null}
     * by returning {@code false}.
     *
     * <pre>
     *   BooleanUtils.toBoolean(Boolean.TRUE)  = true
     *   BooleanUtils.toBoolean(Boolean.FALSE) = false
     *   BooleanUtils.toBoolean(null)          = false
     * </pre>
     *
     * @param bool  the boolean to convert
     * @return {@code true} or {@code false}, {@code null} returns {@code false}
     */
    public static boolean toBoolean(final Boolean bool) {
        return bool != null && bool.booleanValue();
    }

    /**
     * Converts an int to a boolean using the convention that {@code zero}
     * is {@code false}, everything else is {@code true}.
     *
     * <pre>
     *   BooleanUtils.toBoolean(0) = false
     *   BooleanUtils.toBoolean(1) = true
     *   BooleanUtils.toBoolean(2) = true
     * </pre>
     *
     * @param value  the int to convert
     * @return {@code true} if non-zero, {@code false}
     *  if zero
     */
    public static boolean toBoolean(final int value) {
        return value != 0;
    }

    /**
     * Converts an int to a boolean specifying the conversion values.
     *
     * <p>If the {@code trueValue} and {@code falseValue} are the same number then
     * the return value will be {@code true} in case {@code value} matches it.</p>
     *
     * <pre>
     *   BooleanUtils.toBoolean(0, 1, 0) = false
     *   BooleanUtils.toBoolean(1, 1, 0) = true
     *   BooleanUtils.toBoolean(1, 1, 1) = true
     *   BooleanUtils.toBoolean(2, 1, 2) = false
     *   BooleanUtils.toBoolean(2, 2, 0) = true
     * </pre>
     *
     * @param value  the {@link Integer} to convert
     * @param trueValue  the value to match for {@code true}
     * @param falseValue  the value to match for {@code false}
     * @return {@code true} or {@code false}
     * @throws IllegalArgumentException if {@code value} does not match neither
     * {@code trueValue} no {@code falseValue}
     */
    public static boolean toBoolean(final int value, final int trueValue, final int falseValue) {
        if (value == trueValue) {
            return true;
        }
        if (value == falseValue) {
            return false;
        }
        throw new IllegalArgumentException("The Integer did not match either specified value");
    }

    /**
     * Converts an Integer to a boolean specifying the conversion values.
     *
     * <pre>
     *   BooleanUtils.toBoolean(Integer.valueOf(0), Integer.valueOf(1), Integer.valueOf(0)) = false
     *   BooleanUtils.toBoolean(Integer.valueOf(1), Integer.valueOf(1), Integer.valueOf(0)) = true
     *   BooleanUtils.toBoolean(Integer.valueOf(2), Integer.valueOf(1), Integer.valueOf(2)) = false
     *   BooleanUtils.toBoolean(Integer.valueOf(2), Integer.valueOf(2), Integer.valueOf(0)) = true
     *   BooleanUtils.toBoolean(null, null, Integer.valueOf(0))                     = true
     * </pre>
     *
     * @param value  the Integer to convert
     * @param trueValue  the value to match for {@code true}, may be {@code null}
     * @param falseValue  the value to match for {@code false}, may be {@code null}
     * @return {@code true} or {@code false}
     * @throws IllegalArgumentException if no match
     */
    public static boolean toBoolean(final Integer value, final Integer trueValue, final Integer falseValue) {
        if (value == null) {
            if (trueValue == null) {
                return true;
            }
            if (falseValue == null) {
                return false;
            }
        } else if (value.equals(trueValue)) {
            return true;
        } else if (value.equals(falseValue)) {
            return false;
        }
        throw new IllegalArgumentException("The Integer did not match either specified value");
    }

    /**
     * Converts a String to a boolean (optimised for performance).
     *
     * <p>{@code 'true'}, {@code 'on'}, {@code 'y'}, {@code 't'} or {@code 'yes'}
     * (case insensitive) will return {@code true}. Otherwise,
     * {@code false} is returned.</p>
     *
     * <p>This method performs 4 times faster (JDK1.4) than
     * {@code Boolean.valueOf(String)}. However, this method accepts
     * 'on' and 'yes', 't', 'y' as true values.
     *
     * <pre>
     *   BooleanUtils.toBoolean(null)    = false
     *   BooleanUtils.toBoolean("true")  = true
     *   BooleanUtils.toBoolean("TRUE")  = true
     *   BooleanUtils.toBoolean("tRUe")  = true
     *   BooleanUtils.toBoolean("on")    = true
     *   BooleanUtils.toBoolean("yes")   = true
     *   BooleanUtils.toBoolean("false") = false
     *   BooleanUtils.toBoolean("x gti") = false
     *   BooleanUtils.toBoolean("y") = true
     *   BooleanUtils.toBoolean("n") = false
     *   BooleanUtils.toBoolean("t") = true
     *   BooleanUtils.toBoolean("f") = false
     * </pre>
     *
     * @param str  the String to check
     * @return the boolean value of the string, {@code false} if no match or the String is null
     */
    public static boolean toBoolean(final String str) {
        return toBooleanObject(str) == Boolean.TRUE;
    }

    /**
     * Converts a String to a Boolean throwing an exception if no match found.
     *
     * <pre>
     *   BooleanUtils.toBoolean("true", "true", "false")  = true
     *   BooleanUtils.toBoolean("false", "true", "false") = false
     * </pre>
     *
     * @param str  the String to check
     * @param trueString  the String to match for {@code true} (case-sensitive), may be {@code null}
     * @param falseString  the String to match for {@code false} (case-sensitive), may be {@code null}
     * @return the boolean value of the string
     * @throws IllegalArgumentException if the String doesn't match
     */
    public static boolean toBoolean(final String str, final String trueString, final String falseString) {
        if (str == trueString) {
            return true;
        }
        if (str == falseString) {
            return false;
        }
        if (str != null) {
            if (str.equals(trueString)) {
                return true;
            }
            if (str.equals(falseString)) {
                return false;
            }
        }
        throw new IllegalArgumentException("The String did not match either specified value");
    }

    /**
     * Converts a Boolean to a boolean handling {@code null}.
     *
     * <pre>
     *   BooleanUtils.toBooleanDefaultIfNull(Boolean.TRUE, false)  = true
     *   BooleanUtils.toBooleanDefaultIfNull(Boolean.TRUE, true)   = true
     *   BooleanUtils.toBooleanDefaultIfNull(Boolean.FALSE, true)  = false
     *   BooleanUtils.toBooleanDefaultIfNull(Boolean.FALSE, false) = false
     *   BooleanUtils.toBooleanDefaultIfNull(null, true)           = true
     *   BooleanUtils.toBooleanDefaultIfNull(null, false)          = false
     * </pre>
     *
     * @param bool  the boolean object to convert to primitive
     * @param valueIfNull  the boolean value to return if the parameter {@code bool} is {@code null}
     * @return {@code true} or {@code false}
     */
    public static boolean toBooleanDefaultIfNull(final Boolean bool, final boolean valueIfNull) {
        if (bool == null) {
            return valueIfNull;
        }
        return bool.booleanValue();
    }

    /**
     * Converts an int to a Boolean using the convention that {@code zero}
     * is {@code false}, everything else is {@code true}.
     *
     * <pre>
     *   BooleanUtils.toBoolean(0) = Boolean.FALSE
     *   BooleanUtils.toBoolean(1) = Boolean.TRUE
     *   BooleanUtils.toBoolean(2) = Boolean.TRUE
     * </pre>
     *
     * @param value  the int to convert
     * @return Boolean.TRUE if non-zero, Boolean.FALSE if zero,
     *  {@code null} if {@code null}
     */
    public static Boolean toBooleanObject(final int value) {
        return value == 0 ? Boolean.FALSE : Boolean.TRUE;
    }

    /**
     * Converts an int to a Boolean specifying the conversion values.
     *
     * <p>NOTE: This method may return {@code null} and may throw a {@link NullPointerException}
     * if unboxed to a {@code boolean}.</p>
     *
     * <p>The checks are done first for the {@code trueValue}, then for the {@code falseValue} and
     * finally for the {@code nullValue}.</p>
     *
     * <pre>
     *   BooleanUtils.toBooleanObject(0, 0, 2, 3) = Boolean.TRUE
     *   BooleanUtils.toBooleanObject(0, 0, 0, 3) = Boolean.TRUE
     *   BooleanUtils.toBooleanObject(0, 0, 0, 0) = Boolean.TRUE
     *   BooleanUtils.toBooleanObject(2, 1, 2, 3) = Boolean.FALSE
     *   BooleanUtils.toBooleanObject(2, 1, 2, 2) = Boolean.FALSE
     *   BooleanUtils.toBooleanObject(3, 1, 2, 3) = null
     * </pre>
     *
     * @param value  the Integer to convert
     * @param trueValue  the value to match for {@code true}
     * @param falseValue  the value to match for {@code false}
     * @param nullValue  the value to match for {@code null}
     * @return Boolean.TRUE, Boolean.FALSE, or {@code null}
     * @throws IllegalArgumentException if no match
     */
    public static Boolean toBooleanObject(final int value, final int trueValue, final int falseValue, final int nullValue) {
        if (value == trueValue) {
            return Boolean.TRUE;
        }
        if (value == falseValue) {
            return Boolean.FALSE;
        }
        if (value == nullValue) {
            return null;
        }
        throw new IllegalArgumentException("The Integer did not match any specified value");
    }

    /**
     * Converts an Integer to a Boolean using the convention that {@code zero}
     * is {@code false}, every other numeric value is {@code true}.
     *
     * <p>{@code null} will be converted to {@code null}.</p>
     *
     * <p>NOTE: This method may return {@code null} and may throw a {@link NullPointerException}
     * if unboxed to a {@code boolean}.</p>
     *
     * <pre>
     *   BooleanUtils.toBooleanObject(Integer.valueOf(0))    = Boolean.FALSE
     *   BooleanUtils.toBooleanObject(Integer.valueOf(1))    = Boolean.TRUE
     *   BooleanUtils.toBooleanObject(Integer.valueOf(null)) = null
     * </pre>
     *
     * @param value  the Integer to convert
     * @return Boolean.TRUE if non-zero, Boolean.FALSE if zero,
     *  {@code null} if {@code null} input
     */
    public static Boolean toBooleanObject(final Integer value) {
        if (value == null) {
            return null;
        }
        return value.intValue() == 0 ? Boolean.FALSE : Boolean.TRUE;
    }

    /**
     * Converts an Integer to a Boolean specifying the conversion values.
     *
     * <p>NOTE: This method may return {@code null} and may throw a {@link NullPointerException}
     * if unboxed to a {@code boolean}.</p>
     *
     * <p>The checks are done first for the {@code trueValue}, then for the {@code falseValue} and
     * finally for the {@code nullValue}.</p>
     **
     * <pre>
     *   BooleanUtils.toBooleanObject(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(2), Integer.valueOf(3)) = Boolean.TRUE
     *   BooleanUtils.toBooleanObject(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(3)) = Boolean.TRUE
     *   BooleanUtils.toBooleanObject(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(0)) = Boolean.TRUE
     *   BooleanUtils.toBooleanObject(Integer.valueOf(2), Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(3)) = Boolean.FALSE
     *   BooleanUtils.toBooleanObject(Integer.valueOf(2), Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(2)) = Boolean.FALSE
     *   BooleanUtils.toBooleanObject(Integer.valueOf(3), Integer.valueOf(1), Integer.valueOf(2), Integer.valueOf(3)) = null
     * </pre>
     *
     * @param value  the Integer to convert
     * @param trueValue  the value to match for {@code true}, may be {@code null}
     * @param falseValue  the value to match for {@code false}, may be {@code null}
     * @param nullValue  the value to match for {@code null}, may be {@code null}
     * @return Boolean.TRUE, Boolean.FALSE, or {@code null}
     * @throws IllegalArgumentException if no match
     */
    public static Boolean toBooleanObject(final Integer value, final Integer trueValue, final Integer falseValue, final Integer nullValue) {
        if (value == null) {
            if (trueValue == null) {
                return Boolean.TRUE;
            }
            if (falseValue == null) {
                return Boolean.FALSE;
            }
            if (nullValue == null) {
                return null;
            }
        } else if (value.equals(trueValue)) {
            return Boolean.TRUE;
        } else if (value.equals(falseValue)) {
            return Boolean.FALSE;
        } else if (value.equals(nullValue)) {
            return null;
        }
        throw new IllegalArgumentException("The Integer did not match any specified value");
    }

    /**
     * Converts a String to a Boolean.
     *
     * <p>{@code 'true'}, {@code 'on'}, {@code 'y'}, {@code 't'}, {@code 'yes'}
     * or {@code '1'} (case insensitive) will return {@code true}.
     * {@code 'false'}, {@code 'off'}, {@code 'n'}, {@code 'f'}, {@code 'no'}
     * or {@code '0'} (case insensitive) will return {@code false}.
     * Otherwise, {@code null} is returned.</p>
     *
     * <p>NOTE: This method may return {@code null} and may throw a {@link NullPointerException}
     * if unboxed to a {@code boolean}.</p>
     *
     * <pre>
     *   // N.B. case is not significant
     *   BooleanUtils.toBooleanObject(null)    = null
     *   BooleanUtils.toBooleanObject("true")  = Boolean.TRUE
     *   BooleanUtils.toBooleanObject("T")     = Boolean.TRUE // i.e. T[RUE]
     *   BooleanUtils.toBooleanObject("false") = Boolean.FALSE
     *   BooleanUtils.toBooleanObject("f")     = Boolean.FALSE // i.e. f[alse]
     *   BooleanUtils.toBooleanObject("No")    = Boolean.FALSE
     *   BooleanUtils.toBooleanObject("n")     = Boolean.FALSE // i.e. n[o]
     *   BooleanUtils.toBooleanObject("on")    = Boolean.TRUE
     *   BooleanUtils.toBooleanObject("ON")    = Boolean.TRUE
     *   BooleanUtils.toBooleanObject("off")   = Boolean.FALSE
     *   BooleanUtils.toBooleanObject("oFf")   = Boolean.FALSE
     *   BooleanUtils.toBooleanObject("yes")   = Boolean.TRUE
     *   BooleanUtils.toBooleanObject("Y")     = Boolean.TRUE // i.e. Y[ES]
     *   BooleanUtils.toBooleanObject("1")     = Boolean.TRUE
     *   BooleanUtils.toBooleanObject("0")     = Boolean.FALSE
     *   BooleanUtils.toBooleanObject("blue")  = null
     *   BooleanUtils.toBooleanObject("true ") = null // trailing space (too long)
     *   BooleanUtils.toBooleanObject("ono")   = null // does not match on or no
     * </pre>
     *
     * @param str  the String to check; upper and lower case are treated as the same
     * @return the Boolean value of the string, {@code null} if no match or {@code null} input
     */
    public static Boolean toBooleanObject(final String str) {
        // Previously used equalsIgnoreCase, which was fast for interned 'true'.
        // Non interned 'true' matched 15 times slower.
        //
        // Optimisation provides same performance as before for interned 'true'.
        // Similar performance for null, 'false', and other strings not length 2/3/4.
        // 'true'/'TRUE' match 4 times slower, 'tRUE'/'True' 7 times slower.
        if (str == TRUE) {
            return Boolean.TRUE;
        }
        if (str == null) {
            return null;
        }
        switch (str.length()) {
            case 1: {
                final char ch0 = str.charAt(0);
                if (ch0 == 'y' || ch0 == 'Y' ||
                    ch0 == 't' || ch0 == 'T' ||
                    ch0 == '1') {
                    return Boolean.TRUE;
                }
                if (ch0 == 'n' || ch0 == 'N' ||
                    ch0 == 'f' || ch0 == 'F' ||
                    ch0 == '0') {
                    return Boolean.FALSE;
                }
                break;
            }
            case 2: {
                final char ch0 = str.charAt(0);
                final char ch1 = str.charAt(1);
                if ((ch0 == 'o' || ch0 == 'O') &&
                    (ch1 == 'n' || ch1 == 'N') ) {
                    return Boolean.TRUE;
                }
                if ((ch0 == 'n' || ch0 == 'N') &&
                    (ch1 == 'o' || ch1 == 'O') ) {
                    return Boolean.FALSE;
                }
                break;
            }
            case 3: {
                final char ch0 = str.charAt(0);
                final char ch1 = str.charAt(1);
                final char ch2 = str.charAt(2);
                if ((ch0 == 'y' || ch0 == 'Y') &&
                    (ch1 == 'e' || ch1 == 'E') &&
                    (ch2 == 's' || ch2 == 'S') ) {
                    return Boolean.TRUE;
                }
                if ((ch0 == 'o' || ch0 == 'O') &&
                    (ch1 == 'f' || ch1 == 'F') &&
                    (ch2 == 'f' || ch2 == 'F') ) {
                    return Boolean.FALSE;
                }
                break;
            }
            case 4: {
                final char ch0 = str.charAt(0);
                final char ch1 = str.charAt(1);
                final char ch2 = str.charAt(2);
                final char ch3 = str.charAt(3);
                if ((ch0 == 't' || ch0 == 'T') &&
                    (ch1 == 'r' || ch1 == 'R') &&
                    (ch2 == 'u' || ch2 == 'U') &&
                    (ch3 == 'e' || ch3 == 'E') ) {
                    return Boolean.TRUE;
                }
                break;
            }
            case 5: {
                final char ch0 = str.charAt(0);
                final char ch1 = str.charAt(1);
                final char ch2 = str.charAt(2);
                final char ch3 = str.charAt(3);
                final char ch4 = str.charAt(4);
                if ((ch0 == 'f' || ch0 == 'F') &&
                    (ch1 == 'a' || ch1 == 'A') &&
                    (ch2 == 'l' || ch2 == 'L') &&
                    (ch3 == 's' || ch3 == 'S') &&
                    (ch4 == 'e' || ch4 == 'E') ) {
                    return Boolean.FALSE;
                }
                break;
            }
        default:
            break;
        }

        return null;
    }

    /**
     * Converts a String to a Boolean throwing an exception if no match.
     *
     * <p>NOTE: This method may return {@code null} and may throw a {@link NullPointerException}
     * if unboxed to a {@code boolean}.</p>
     *
     * <pre>
     *   BooleanUtils.toBooleanObject("true", "true", "false", "null")   = Boolean.TRUE
     *   BooleanUtils.toBooleanObject(null, null, "false", "null")       = Boolean.TRUE
     *   BooleanUtils.toBooleanObject(null, null, null, "null")          = Boolean.TRUE
     *   BooleanUtils.toBooleanObject(null, null, null, null)            = Boolean.TRUE
     *   BooleanUtils.toBooleanObject("false", "true", "false", "null")  = Boolean.FALSE
     *   BooleanUtils.toBooleanObject("false", "true", "false", "false") = Boolean.FALSE
     *   BooleanUtils.toBooleanObject(null, "true", null, "false")       = Boolean.FALSE
     *   BooleanUtils.toBooleanObject(null, "true", null, null)          = Boolean.FALSE
     *   BooleanUtils.toBooleanObject("null", "true", "false", "null")   = null
     * </pre>
     *
     * @param str  the String to check
     * @param trueString  the String to match for {@code true} (case-sensitive), may be {@code null}
     * @param falseString  the String to match for {@code false} (case-sensitive), may be {@code null}
     * @param nullString  the String to match for {@code null} (case-sensitive), may be {@code null}
     * @return the Boolean value of the string, {@code null} if either the String matches {@code nullString}
     *  or if {@code null} input and {@code nullString} is {@code null}
     * @throws IllegalArgumentException if the String doesn't match
     */
    public static Boolean toBooleanObject(final String str, final String trueString, final String falseString, final String nullString) {
        if (str == null) {
            if (trueString == null) {
                return Boolean.TRUE;
            }
            if (falseString == null) {
                return Boolean.FALSE;
            }
            if (nullString == null) {
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

    /**
     * Converts a boolean to an int using the convention that
     * {@code true} is {@code 1} and {@code false} is {@code 0}.
     *
     * <pre>
     *   BooleanUtils.toInteger(true)  = 1
     *   BooleanUtils.toInteger(false) = 0
     * </pre>
     *
     * @param bool  the boolean to convert
     * @return one if {@code true}, zero if {@code false}
     */
    public static int toInteger(final boolean bool) {
        return bool ? 1 : 0;
    }

    /**
     * Converts a boolean to an int specifying the conversion values.
     *
     * <pre>
     *   BooleanUtils.toInteger(true, 1, 0)  = 1
     *   BooleanUtils.toInteger(false, 1, 0) = 0
     * </pre>
     *
     * @param bool  the to convert
     * @param trueValue  the value to return if {@code true}
     * @param falseValue  the value to return if {@code false}
     * @return the appropriate value
     */
    public static int toInteger(final boolean bool, final int trueValue, final int falseValue) {
        return bool ? trueValue : falseValue;
    }

    /**
     * Converts a Boolean to an int specifying the conversion values.
     *
     * <pre>
     *   BooleanUtils.toInteger(Boolean.TRUE, 1, 0, 2)  = 1
     *   BooleanUtils.toInteger(Boolean.FALSE, 1, 0, 2) = 0
     *   BooleanUtils.toInteger(null, 1, 0, 2)          = 2
     * </pre>
     *
     * @param bool  the Boolean to convert
     * @param trueValue  the value to return if {@code true}
     * @param falseValue  the value to return if {@code false}
     * @param nullValue  the value to return if {@code null}
     * @return the appropriate value
     */
    public static int toInteger(final Boolean bool, final int trueValue, final int falseValue, final int nullValue) {
        if (bool == null) {
            return nullValue;
        }
        return bool.booleanValue() ? trueValue : falseValue;
    }

    /**
     * Converts a boolean to an Integer using the convention that
     * {@code true} is {@code 1} and {@code false} is {@code 0}.
     *
     * <pre>
     *   BooleanUtils.toIntegerObject(true)  = Integer.valueOf(1)
     *   BooleanUtils.toIntegerObject(false) = Integer.valueOf(0)
     * </pre>
     *
     * @param bool  the boolean to convert
     * @return one if {@code true}, zero if {@code false}
     */
    public static Integer toIntegerObject(final boolean bool) {
        return bool ? NumberUtils.INTEGER_ONE : NumberUtils.INTEGER_ZERO;
    }

    /**
     * Converts a boolean to an Integer specifying the conversion values.
     *
     * <pre>
     *   BooleanUtils.toIntegerObject(true, Integer.valueOf(1), Integer.valueOf(0))  = Integer.valueOf(1)
     *   BooleanUtils.toIntegerObject(false, Integer.valueOf(1), Integer.valueOf(0)) = Integer.valueOf(0)
     * </pre>
     *
     * @param bool  the to convert
     * @param trueValue  the value to return if {@code true}, may be {@code null}
     * @param falseValue  the value to return if {@code false}, may be {@code null}
     * @return the appropriate value
     */
    public static Integer toIntegerObject(final boolean bool, final Integer trueValue, final Integer falseValue) {
        return bool ? trueValue : falseValue;
    }

    /**
     * Converts a Boolean to a Integer using the convention that
     * {@code zero} is {@code false}.
     *
     * <p>{@code null} will be converted to {@code null}.</p>
     *
     * <pre>
     *   BooleanUtils.toIntegerObject(Boolean.TRUE)  = Integer.valueOf(1)
     *   BooleanUtils.toIntegerObject(Boolean.FALSE) = Integer.valueOf(0)
     * </pre>
     *
     * @param bool  the Boolean to convert
     * @return one if Boolean.TRUE, zero if Boolean.FALSE, {@code null} if {@code null}
     */
    public static Integer toIntegerObject(final Boolean bool) {
        if (bool == null) {
            return null;
        }
        return bool.booleanValue() ? NumberUtils.INTEGER_ONE : NumberUtils.INTEGER_ZERO;
    }

    /**
     * Converts a Boolean to an Integer specifying the conversion values.
     *
     * <pre>
     *   BooleanUtils.toIntegerObject(Boolean.TRUE, Integer.valueOf(1), Integer.valueOf(0), Integer.valueOf(2))  = Integer.valueOf(1)
     *   BooleanUtils.toIntegerObject(Boolean.FALSE, Integer.valueOf(1), Integer.valueOf(0), Integer.valueOf(2)) = Integer.valueOf(0)
     *   BooleanUtils.toIntegerObject(null, Integer.valueOf(1), Integer.valueOf(0), Integer.valueOf(2))          = Integer.valueOf(2)
     * </pre>
     *
     * @param bool  the Boolean to convert
     * @param trueValue  the value to return if {@code true}, may be {@code null}
     * @param falseValue  the value to return if {@code false}, may be {@code null}
     * @param nullValue  the value to return if {@code null}, may be {@code null}
     * @return the appropriate value
     */
    public static Integer toIntegerObject(final Boolean bool, final Integer trueValue, final Integer falseValue, final Integer nullValue) {
        if (bool == null) {
            return nullValue;
        }
        return bool.booleanValue() ? trueValue : falseValue;
    }

    /**
     * Converts a boolean to a String returning one of the input Strings.
     *
     * <pre>
     *   BooleanUtils.toString(true, "true", "false")   = "true"
     *   BooleanUtils.toString(false, "true", "false")  = "false"
     * </pre>
     *
     * @param bool  the Boolean to check
     * @param trueString  the String to return if {@code true}, may be {@code null}
     * @param falseString  the String to return if {@code false}, may be {@code null}
     * @return one of the two input Strings
     */
    public static String toString(final boolean bool, final String trueString, final String falseString) {
        return bool ? trueString : falseString;
    }

    /**
     * Converts a Boolean to a String returning one of the input Strings.
     *
     * <pre>
     *   BooleanUtils.toString(Boolean.TRUE, "true", "false", null)   = "true"
     *   BooleanUtils.toString(Boolean.FALSE, "true", "false", null)  = "false"
     *   BooleanUtils.toString(null, "true", "false", null)           = null;
     * </pre>
     *
     * @param bool  the Boolean to check
     * @param trueString  the String to return if {@code true}, may be {@code null}
     * @param falseString  the String to return if {@code false}, may be {@code null}
     * @param nullString  the String to return if {@code null}, may be {@code null}
     * @return one of the three input Strings
     */
    public static String toString(final Boolean bool, final String trueString, final String falseString, final String nullString) {
        if (bool == null) {
            return nullString;
        }
        return bool.booleanValue() ? trueString : falseString;
    }

    /**
     * Converts a boolean to a String returning {@code 'on'}
     * or {@code 'off'}.
     *
     * <pre>
     *   BooleanUtils.toStringOnOff(true)   = "on"
     *   BooleanUtils.toStringOnOff(false)  = "off"
     * </pre>
     *
     * @param bool  the Boolean to check
     * @return {@code 'on'}, {@code 'off'}, or {@code null}
     */
    public static String toStringOnOff(final boolean bool) {
        return toString(bool, ON, OFF);
    }

    /**
     * Converts a Boolean to a String returning {@code 'on'},
     * {@code 'off'}, or {@code null}.
     *
     * <pre>
     *   BooleanUtils.toStringOnOff(Boolean.TRUE)  = "on"
     *   BooleanUtils.toStringOnOff(Boolean.FALSE) = "off"
     *   BooleanUtils.toStringOnOff(null)          = null;
     * </pre>
     *
     * @param bool  the Boolean to check
     * @return {@code 'on'}, {@code 'off'}, or {@code null}
     */
    public static String toStringOnOff(final Boolean bool) {
        return toString(bool, ON, OFF, null);
    }

    /**
     * Converts a boolean to a String returning {@code 'true'}
     * or {@code 'false'}.
     *
     * <pre>
     *   BooleanUtils.toStringTrueFalse(true)   = "true"
     *   BooleanUtils.toStringTrueFalse(false)  = "false"
     * </pre>
     *
     * @param bool  the Boolean to check
     * @return {@code 'true'}, {@code 'false'}, or {@code null}
     */
    public static String toStringTrueFalse(final boolean bool) {
        return toString(bool, TRUE, FALSE);
    }

    /**
     * Converts a Boolean to a String returning {@code 'true'},
     * {@code 'false'}, or {@code null}.
     *
     * <pre>
     *   BooleanUtils.toStringTrueFalse(Boolean.TRUE)  = "true"
     *   BooleanUtils.toStringTrueFalse(Boolean.FALSE) = "false"
     *   BooleanUtils.toStringTrueFalse(null)          = null;
     * </pre>
     *
     * @param bool  the Boolean to check
     * @return {@code 'true'}, {@code 'false'}, or {@code null}
     */
    public static String toStringTrueFalse(final Boolean bool) {
        return toString(bool, TRUE, FALSE, null);
    }

    /**
     * Converts a boolean to a String returning {@code 'yes'}
     * or {@code 'no'}.
     *
     * <pre>
     *   BooleanUtils.toStringYesNo(true)   = "yes"
     *   BooleanUtils.toStringYesNo(false)  = "no"
     * </pre>
     *
     * @param bool  the Boolean to check
     * @return {@code 'yes'}, {@code 'no'}, or {@code null}
     */
    public static String toStringYesNo(final boolean bool) {
        return toString(bool, YES, NO);
    }

    /**
     * Converts a Boolean to a String returning {@code 'yes'},
     * {@code 'no'}, or {@code null}.
     *
     * <pre>
     *   BooleanUtils.toStringYesNo(Boolean.TRUE)  = "yes"
     *   BooleanUtils.toStringYesNo(Boolean.FALSE) = "no"
     *   BooleanUtils.toStringYesNo(null)          = null;
     * </pre>
     *
     * @param bool  the Boolean to check
     * @return {@code 'yes'}, {@code 'no'}, or {@code null}
     */
    public static String toStringYesNo(final Boolean bool) {
        return toString(bool, YES, NO, null);
    }

    /**
     * Returns an unmodifiable list of Booleans {@code [false, true]}.
     *
     * @return an unmodifiable list of Booleans {@code [false, true]}.
     * @since 3.13.0
     */
    public static List<Boolean> values() {
        return BOOLEAN_LIST;
    }

    /**
     * Performs an xor on a set of booleans.
     * <p>
     *   This behaves like an XOR gate;
     *   it returns true if the number of true values is odd,
     *   and false if the number of true values is zero or even.
     * </p>
     *
     * <pre>
     *   BooleanUtils.xor(true, true)             = false
     *   BooleanUtils.xor(false, false)           = false
     *   BooleanUtils.xor(true, false)            = true
     *   BooleanUtils.xor(true, false, false)     = true
     *   BooleanUtils.xor(true, true, true)       = true
     *   BooleanUtils.xor(true, true, true, true) = false
     * </pre>
     *
     * @param array  an array of {@code boolean}s
     * @return true if the number of true values in the array is odd; otherwise returns false.
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty.
     */
    public static boolean xor(final boolean... array) {
        ObjectUtils.requireNonEmpty(array, "array");
        // false if the neutral element of the xor operator
        boolean result = false;
        for (final boolean element : array) {
            result ^= element;
        }

        return result;
    }

    /**
     * Performs an xor on an array of Booleans.
     * <pre>
     *   BooleanUtils.xor(Boolean.TRUE, Boolean.TRUE)                 = Boolean.FALSE
     *   BooleanUtils.xor(Boolean.FALSE, Boolean.FALSE)               = Boolean.FALSE
     *   BooleanUtils.xor(Boolean.TRUE, Boolean.FALSE)                = Boolean.TRUE
     *   BooleanUtils.xor(Boolean.TRUE, Boolean.FALSE, Boolean.FALSE) = Boolean.TRUE
     *   BooleanUtils.xor(Boolean.FALSE, null)                        = Boolean.FALSE
     *   BooleanUtils.xor(Boolean.TRUE, null)                         = Boolean.TRUE
     * </pre>
     * <p>
     * Null array elements map to false, like {@code Boolean.parseBoolean(null)} and its callers return false.
     * </p>
     *
     * @param array  an array of {@link Boolean}s
     * @return the result of the xor operations
     * @throws NullPointerException if {@code array} is {@code null}
     * @throws IllegalArgumentException if {@code array} is empty.
     */
    public static Boolean xor(final Boolean... array) {
        ObjectUtils.requireNonEmpty(array, "array");
        return xor(ArrayUtils.toPrimitive(array)) ? Boolean.TRUE : Boolean.FALSE;
    }

    /**
     * {@link BooleanUtils} instances should NOT be constructed in standard programming.
     * Instead, the class should be used as {@code BooleanUtils.negate(true);}.
     *
     * <p>This constructor is public to permit tools that require a JavaBean instance
     * to operate.</p>
     */
    public BooleanUtils() {
    }

}
