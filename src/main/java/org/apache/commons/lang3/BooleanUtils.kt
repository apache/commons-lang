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
package org.apache.commons.lang3

/**
 * Operations on boolean primitives and Boolean objects.
 *
 * This class tries to handle `null` input gracefully.
 * An exception will not be thrown for a `null` input.
 * Each method documents its behavior in more detail.
 *
 * @since 2.0
 */
open class BooleanUtils {
    companion object {
        /**
         * Negates the `Boolean`. If it is `null`, `null` will be returned.
         */
        @JvmStatic
        fun Boolean?.negate() = this?.not()

        // boolean Boolean methods
        //-----------------------------------------------------------------------
        /**
         * Checks if a `Boolean` value is `true`, handling `null` by returning `false`.
         */
        @JvmStatic
        fun Boolean?.isTrue() = this == true

        /**
         * Checks if a `Boolean` value is *not* `true`, handling `null` by returning `true`.
         */
        @JvmStatic
        fun Boolean?.isNotTrue() = !isTrue()

        /**
         * Checks if a `Boolean` value is `false`, handling `null` by returning `false`.
         *
         * @since 2.1
         */
        @JvmStatic
        fun Boolean?.isFalse() = this == false

        /**
         * Checks if a `Boolean` value is *not* `false`, handling `null` by returning `true`.
         */
        @JvmStatic
        fun Boolean?.isNotFalse() = !isFalse()

        //-----------------------------------------------------------------------
        /**
         * Converts a `Boolean` to a `boolean`, handling `null` by returning `false`.
         */
        @JvmStatic
        fun Boolean?.toBoolean() = this != null && this

        /**
         * Converts a `Boolean` to a `boolean`, handling `null` by returning [valueIfNull].
         */
        @JvmStatic
        fun Boolean?.toBooleanDefaultIfNull(valueIfNull: Boolean) = this ?: valueIfNull

        // Integer to Boolean methods
        //-----------------------------------------------------------------------
        /**
         * Converts an [Int] to a boolean using the convention that `zero`
         * is `false` and everything else is `true`.
         */
        @JvmStatic
        fun Int.toBoolean() = this != 0

        /**
         * Converts an `int` to a Boolean using the convention that `zero`
         * is `false`, everything else is `true`.
         */
        @JvmStatic
        fun Int.toBooleanObject(): Boolean? = toBoolean()

        /**
         * Converts an Integer to a Boolean using the convention that `zero`
         * is `false`, every other numeric value is `true`.
         *
         * `null` will be converted to `null`.
         */
        @JvmStatic
        fun Int?.toBooleanObject() = this?.toBoolean()

        /**
         * Converts an Int to a boolean, specifying the conversion values.
         *
         * If [trueValue] and [falseValue] are the same number, then
         * the return value will be `true` if the Int value is equal to it.
         *
         * @throws IllegalArgumentException if `value` does not match either `trueValue` or `falseValue`
         */
        @JvmStatic
        fun Int.toBoolean(trueValue: Int, falseValue: Int) = (this as Int?).toBoolean(trueValue, falseValue)

        /**
         * Converts an Integer to a boolean, specifying the conversion values.
         *
         * @throws IllegalArgumentException if no match
         */
        @JvmStatic
        fun Int?.toBoolean(trueValue: Int?, falseValue: Int?) = when {
            this == trueValue -> true
            this == falseValue -> false
            else -> throw IllegalArgumentException("The Integer did not match either specified value")
        }

        /**
         * Converts an int to a Boolean, specifying the conversion values.
         *
         * NOTE: This method may return `null` and may throw a `NullPointerException`
         * if unboxed to a `boolean`.
         *
         * The checks are done first for the [trueValue], then for the [falseValue] and
         * finally for the [nullValue].
         *
         * @throws IllegalArgumentException if no match
         */
        @JvmStatic
        fun Int.toBooleanObject(trueValue: Int, falseValue: Int, nullValue: Int) =
                (this as Int?).toBooleanObject(trueValue, falseValue, nullValue)

        /**
         * Converts an Integer to a Boolean, specifying the conversion values.
         *
         * The checks are done first for the [trueValue], then for the [falseValue] and
         * finally for the [nullValue].
         *
         * @throws IllegalArgumentException if no match
         */
        @JvmStatic
        fun Int?.toBooleanObject(trueValue: Int?, falseValue: Int?, nullValue: Int?) = when (this) {
            trueValue -> true
            falseValue -> false
            nullValue -> null
            else -> throw IllegalArgumentException("The Integer did not match any specified value")
        }

        // Boolean to Integer methods
        //-----------------------------------------------------------------------
        /**
         * Converts a boolean to an int using the convention that
         * `true` is `1` and `false` is `0`.
         */
        @JvmStatic
        fun Boolean.toInteger() = toInteger(1, 0)

        /**
         * Converts a boolean to an Integer using the convention that
         * `true` is `1` and `false` is `0`.
         */
        @JvmStatic
        fun Boolean.toIntegerObject(): Int? = toInteger()

        /**
         * Converts a Boolean to a Integer using the convention that `zero` is `false`.
         *
         * `null` will be converted to `null`.
         */
        @JvmStatic
        fun Boolean?.toIntegerObject() = this?.toIntegerObject()

        /**
         * Converts a boolean to an int, specifying the conversion values.
         */
        @JvmStatic
        fun Boolean.toInteger(trueValue: Int, falseValue: Int) = if (this) trueValue else falseValue

        /**
         * Converts a Boolean to an int, specifying the conversion values.
         */
        @JvmStatic
        fun Boolean?.toInteger(trueValue: Int, falseValue: Int, nullValue: Int) =
                this?.toInteger(trueValue, falseValue) ?: nullValue

        /**
         * Converts a boolean to an Integer, specifying the conversion values.
         */
        @JvmStatic
        fun Boolean.toIntegerObject(trueValue: Int?, falseValue: Int?) = if (this) trueValue else falseValue

        /**
         * Converts a Boolean to an Integer, specifying the conversion values.
         */
        @JvmStatic
        fun Boolean?.toIntegerObject(trueValue: Int?, falseValue: Int?, nullValue: Int?) =
                this?.toIntegerObject(trueValue, falseValue) ?: nullValue

        // String to Boolean methods
        //-----------------------------------------------------------------------
        /**
         * Converts a String to a Boolean.
         *
         * `'true'`, `'on'`, `'y'`, `'t'`, `'yes'` or `'1'` (case insensitive) will return `true`.
         * `'false'`, `'off'`, `'n'`, `'f'`, `'no'` or `'0'` (case insensitive) will return `false`.
         * Otherwise, `null` is returned.
         *
         * NOTE: This method may return `null` and may throw a `NullPointerException`
         * if unboxed to a `boolean`.
         */
        @JvmStatic
        fun String?.toBooleanObject(): Boolean? {
            when (this?.length) {
                1 -> {
                    val ch0 = this[0]
                    if (ch0 in "yYtT1") {
                        return true
                    }
                    if (ch0 in "nNfF0") {
                        return false
                    }
                }
                2 -> {
                    val ch0 = this[0]
                    val ch1 = this[1]
                    if (ch0 in "oO" && ch1 in "nN") {
                        return true
                    } else if (ch0 in "nN" && ch1 in "oO") {
                        return false
                    }
                }
                3 -> {
                    val ch0 = this[0]
                    val ch1 = this[1]
                    val ch2 = this[2]
                    if (ch0 in "yY" && ch1 in "eE" && ch2 in "sS") {
                        return true
                    } else if (ch0 in "oO" && ch1 in "fF" && ch2 in "fF") {
                        return false
                    }
                }
                4 -> {
                    val ch0 = this[0]
                    val ch1 = this[1]
                    val ch2 = this[2]
                    val ch3 = this[3]
                    if (ch0 in "tT" && ch1 in "rR" && ch2 in "uU" && ch3 in "eE") {
                        return true
                    }
                }
                5 -> {
                    val ch0 = this[0]
                    val ch1 = this[1]
                    val ch2 = this[2]
                    val ch3 = this[3]
                    val ch4 = this[4]
                    if (ch0 in "fF" && ch1 in "aA" && ch2 in "lL" && ch3 in "sS" && ch4 in "eE") {
                        return false
                    }
                }
            }
            return null
        }

        /**
         * Converts a String to a Boolean, returning the appropriate value based on the string matched and throwing an
         * [IllegalArgumentException] if it does not match [trueString], [falseString] or [nullString].
         */
        @JvmStatic
        fun String?.toBooleanObject(trueString: String?, falseString: String?, nullString: String?) = when (this) {
            trueString -> true
            falseString -> false
            nullString -> null
            else -> throw IllegalArgumentException("The String did not match any specified value")
        }

        // String to boolean methods
        //-----------------------------------------------------------------------
        /**
         * Converts a String to a boolean (optimised for performance).
         *
         * `'true'`, `'on'`, `'y'`, `'t'` or `'yes'` (case insensitive) will return `true`. Otherwise,
         * `false` is returned.
         *
         * This method performs 4 times faster (JDK1.4) than `Boolean.valueOf(String)`. However, this method
         * accepts 'on', 'yes', 't', 'y' as true values.
         */
        @JvmStatic
        fun String?.toBoolean() = toBooleanObject() == true

        /**
         * Converts a String to a Boolean, throwing an [IllegalArgumentException] if it does not match
         * [trueString] or [falseString].
         */
        @JvmStatic
        fun String?.toBoolean(trueString: String?, falseString: String?) = when {
            this == trueString -> true
            this == falseString -> false
            else -> throw IllegalArgumentException("The String did not match either specified value")
        }

        // Boolean to String methods
        //-----------------------------------------------------------------------
        /**
         * Converts a Boolean to a String, returning `'true'`, `'false'` or `null`.
         */
        @JvmStatic
        fun Boolean?.toStringTrueFalse() = toString("true", "false", null)

        /**
         * Converts a Boolean to a String, returning `'on'`, `'off'` or `null`.
         */
        @JvmStatic
        fun Boolean?.toStringOnOff() = toString("on", "off", null)

        /**
         * Converts a Boolean to a String, returning `'yes'`, `'no'` or `null`.
         */
        @JvmStatic
        fun Boolean?.toStringYesNo() = toString("yes", "no", null)

        /**
         * Converts a Boolean to a String, returning [trueString] if true, [falseString] if false and
         * [nullString] if null.
         */
        @JvmStatic
        fun Boolean?.toString(trueString: String?, falseString: String?, nullString: String?) =
                this?.toString(trueString, falseString) ?: nullString

        /**
         * Converts a boolean to a String, returning `'true'` or `'false'`.
         */
        @JvmStatic
        fun Boolean.toStringTrueFalse() = toString("true", "false")

        /**
         * Converts a boolean to a String, returning `'on'` or `'off'`.
         */
        @JvmStatic
        fun Boolean.toStringOnOff() = toString("on", "off")

        /**
         * Converts a boolean to a String, returning `'yes'` or `'no'`.
         */
        @JvmStatic
        fun Boolean.toStringYesNo() = toString("yes", "no")

        /**
         * Converts a boolean to a String, returning one of the input Strings.
         */
        @JvmStatic
        fun Boolean.toString(trueString: String?, falseString: String?) = if (this) trueString else falseString

        // logical operations
        // ----------------------------------------------------------------------
        private const val EMPTY_ARRAY = "Array is empty"

        private fun Array<out Boolean?>.filterNonNullBools() = filterNotNull().toBooleanArray()
        /**
         * Performs an 'and' operation on a [BooleanArray] and returns the result.
         *
         * @throws IllegalArgumentException if `array` is `null` or empty.
         * @since 3.0.1
         */
        @JvmStatic
        fun and(vararg array: Boolean): Boolean {
            require(array.isNotEmpty()) { EMPTY_ARRAY }
            return array.all { it }
        }

        /**
         * Performs an 'and' operation on an array of Booleans. Null elements are filtered out.
         *
         * @throws IllegalArgumentException if `array` is `null` or empty.
         * @since 3.0.1
         */
        @JvmStatic
        fun and(vararg array: Boolean?): Boolean? {
            require(array.isNotEmpty()) { EMPTY_ARRAY }
            return and(*array.filterNonNullBools())
        }

        /**
         * Performs an 'or' operation on a [BooleanArray].
         *
         * @throws IllegalArgumentException if `array` is `null` or empty.
         * @since 3.0.1
         */
        @JvmStatic
        fun or(vararg array: Boolean): Boolean {
            require(array.isNotEmpty()) { EMPTY_ARRAY }
            return array.any { it }
        }

        /**
         * Performs an 'or' operation on an array of Booleans and returns the result. Null elements are filtered out.
         *
         * @throws IllegalArgumentException if `array` is `null` or empty.
         * @since 3.0.1
         */
        @JvmStatic
        fun or(vararg array: Boolean?): Boolean? {
            require(array.isNotEmpty()) { EMPTY_ARRAY }
            return or(*array.filterNonNullBools())
        }

        /**
         * Performs an xor on a [BooleanArray] and returns the result.
         *
         * @throws IllegalArgumentException if `array` is `null` or empty.
         */
        @JvmStatic
        fun xor(vararg array: Boolean): Boolean {
            require(array.isNotEmpty()) { EMPTY_ARRAY }
            // false if the neutral element of the xor operator
            return array.reduce { result, b -> result xor b }
        }

        /**
         * Performs an xor on a set of Booleans and returns the result. Null elements are filtered out.
         *
         * @throws IllegalArgumentException if `array` is `null` or empty
         */
        @JvmStatic
        fun xor(vararg array: Boolean?): Boolean? {
            require(array.isNotEmpty()) { EMPTY_ARRAY }
            return xor(*array.filterNonNullBools())
        }

        /**
         * Compares two boolean values. This is the same functionality as provided in Java 7.
         *
         * @since 3.4
         */
        @JvmStatic
        fun compare(x: Boolean, y: Boolean) = x.compareTo(y)
    }
}
