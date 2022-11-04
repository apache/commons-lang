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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Utility library to provide helper methods for Java enums.
 *
 * <p>#ThreadSafe#</p>
 *
 * @since 3.0
 */
public class EnumUtils {

    private static final String CANNOT_STORE_S_S_VALUES_IN_S_BITS = "Cannot store %s %s values in %s bits";
    private static final String ENUM_CLASS_MUST_BE_DEFINED = "EnumClass must be defined.";
    private static final String NULL_ELEMENTS_NOT_PERMITTED = "null elements not permitted";
    private static final String S_DOES_NOT_SEEM_TO_BE_AN_ENUM_TYPE = "%s does not seem to be an Enum type";

    /**
     * Validate {@code enumClass}.
     * @param <E> the type of the enumeration
     * @param enumClass to check
     * @return {@code enumClass}
     * @throws NullPointerException if {@code enumClass} is {@code null}
     * @throws IllegalArgumentException if {@code enumClass} is not an enum class
     * @since 3.2
     */
    private static <E extends Enum<E>> Class<E> asEnum(final Class<E> enumClass) {
        Objects.requireNonNull(enumClass, ENUM_CLASS_MUST_BE_DEFINED);
        Validate.isTrue(enumClass.isEnum(), S_DOES_NOT_SEEM_TO_BE_AN_ENUM_TYPE, enumClass);
        return enumClass;
    }

    /**
     * Validate that {@code enumClass} is compatible with representation in a {@code long}.
     * @param <E> the type of the enumeration
     * @param enumClass to check
     * @return {@code enumClass}
     * @throws NullPointerException if {@code enumClass} is {@code null}
     * @throws IllegalArgumentException if {@code enumClass} is not an enum class or has more than 64 values
     * @since 3.0.1
     */
    private static <E extends Enum<E>> Class<E> checkBitVectorable(final Class<E> enumClass) {
        final E[] constants = asEnum(enumClass).getEnumConstants();
        Validate.isTrue(constants.length <= Long.SIZE, CANNOT_STORE_S_S_VALUES_IN_S_BITS,
            Integer.valueOf(constants.length), enumClass.getSimpleName(), Integer.valueOf(Long.SIZE));

        return enumClass;
    }

    /**
     * Creates a long bit vector representation of the given array of Enum values.
     *
     * <p>This generates a value that is usable by {@link EnumUtils#processBitVector}.</p>
     *
     * <p>Do not use this method if you have more than 64 values in your Enum, as this
     * would create a value greater than a long can hold.</p>
     *
     * @param enumClass the class of the enum we are working with, not {@code null}
     * @param values    the values we want to convert, not {@code null}
     * @param <E>       the type of the enumeration
     * @return a long whose value provides a binary representation of the given set of enum values.
     * @throws NullPointerException if {@code enumClass} or {@code values} is {@code null}
     * @throws IllegalArgumentException if {@code enumClass} is not an enum class or has more than 64 values
     * @since 3.0.1
     * @see #generateBitVectors(Class, Iterable)
     */
    @SafeVarargs
    public static <E extends Enum<E>> long generateBitVector(final Class<E> enumClass, final E... values) {
        Validate.noNullElements(values);
        return generateBitVector(enumClass, Arrays.asList(values));
    }

    /**
     * Creates a long bit vector representation of the given subset of an Enum.
     *
     * <p>This generates a value that is usable by {@link EnumUtils#processBitVector}.</p>
     *
     * <p>Do not use this method if you have more than 64 values in your Enum, as this
     * would create a value greater than a long can hold.</p>
     *
     * @param enumClass the class of the enum we are working with, not {@code null}
     * @param values    the values we want to convert, not {@code null}, neither containing {@code null}
     * @param <E>       the type of the enumeration
     * @return a long whose value provides a binary representation of the given set of enum values.
     * @throws NullPointerException if {@code enumClass} or {@code values} is {@code null}
     * @throws IllegalArgumentException if {@code enumClass} is not an enum class or has more than 64 values,
     *                                  or if any {@code values} {@code null}
     * @since 3.0.1
     * @see #generateBitVectors(Class, Iterable)
     */
    public static <E extends Enum<E>> long generateBitVector(final Class<E> enumClass, final Iterable<? extends E> values) {
        checkBitVectorable(enumClass);
        Objects.requireNonNull(values, "values");
        long total = 0;
        for (final E constant : values) {
            Objects.requireNonNull(constant, NULL_ELEMENTS_NOT_PERMITTED);
            total |= 1L << constant.ordinal();
        }
        return total;
    }

    /**
     * Creates a bit vector representation of the given subset of an Enum using as many {@code long}s as needed.
     *
     * <p>This generates a value that is usable by {@link EnumUtils#processBitVectors}.</p>
     *
     * <p>Use this method if you have more than 64 values in your Enum.</p>
     *
     * @param enumClass the class of the enum we are working with, not {@code null}
     * @param values    the values we want to convert, not {@code null}, neither containing {@code null}
     * @param <E>       the type of the enumeration
     * @return a long[] whose values provide a binary representation of the given set of enum values
     *         with the least significant digits rightmost.
     * @throws NullPointerException if {@code enumClass} or {@code values} is {@code null}
     * @throws IllegalArgumentException if {@code enumClass} is not an enum class, or if any {@code values} {@code null}
     * @since 3.2
     */
    @SafeVarargs
    public static <E extends Enum<E>> long[] generateBitVectors(final Class<E> enumClass, final E... values) {
        asEnum(enumClass);
        Validate.noNullElements(values);
        final EnumSet<E> condensed = EnumSet.noneOf(enumClass);
        Collections.addAll(condensed, values);
        final long[] result = new long[(enumClass.getEnumConstants().length - 1) / Long.SIZE + 1];
        for (final E value : condensed) {
            result[value.ordinal() / Long.SIZE] |= 1L << (value.ordinal() % Long.SIZE);
        }
        ArrayUtils.reverse(result);
        return result;
    }

    /**
     * Creates a bit vector representation of the given subset of an Enum using as many {@code long}s as needed.
     *
     * <p>This generates a value that is usable by {@link EnumUtils#processBitVectors}.</p>
     *
     * <p>Use this method if you have more than 64 values in your Enum.</p>
     *
     * @param enumClass the class of the enum we are working with, not {@code null}
     * @param values    the values we want to convert, not {@code null}, neither containing {@code null}
     * @param <E>       the type of the enumeration
     * @return a long[] whose values provide a binary representation of the given set of enum values
     *         with the least significant digits rightmost.
     * @throws NullPointerException if {@code enumClass} or {@code values} is {@code null}
     * @throws IllegalArgumentException if {@code enumClass} is not an enum class, or if any {@code values} {@code null}
     * @since 3.2
     */
    public static <E extends Enum<E>> long[] generateBitVectors(final Class<E> enumClass, final Iterable<? extends E> values) {
        asEnum(enumClass);
        Objects.requireNonNull(values, "values");
        final EnumSet<E> condensed = EnumSet.noneOf(enumClass);
        values.forEach(constant -> condensed.add(Objects.requireNonNull(constant, NULL_ELEMENTS_NOT_PERMITTED)));
        final long[] result = new long[(enumClass.getEnumConstants().length - 1) / Long.SIZE + 1];
        for (final E value : condensed) {
            result[value.ordinal() / Long.SIZE] |= 1L << (value.ordinal() % Long.SIZE);
        }
        ArrayUtils.reverse(result);
        return result;
    }

    /**
     * Gets the enum for the class, returning {@code null} if not found.
     *
     * <p>This method differs from {@link Enum#valueOf} in that it does not throw an exception
     * for an invalid enum name.</p>
     *
     * @param <E> the type of the enumeration
     * @param enumClass  the class of the enum to query, not null
     * @param enumName   the enum name, null returns null
     * @return the enum, null if not found
     */
    public static <E extends Enum<E>> E getEnum(final Class<E> enumClass, final String enumName) {
        return getEnum(enumClass, enumName, null);
    }

    /**
     * Gets the enum for the class, returning {@code defaultEnum} if not found.
     *
     * <p>This method differs from {@link Enum#valueOf} in that it does not throw an exception
     * for an invalid enum name.</p>
     *
     * @param <E> the type of the enumeration
     * @param enumClass   the class of the enum to query, not null
     * @param enumName    the enum name, null returns default enum
     * @param defaultEnum the default enum
     * @return the enum, default enum if not found
     * @since 3.10
     */
    public static <E extends Enum<E>> E getEnum(final Class<E> enumClass, final String enumName, final E defaultEnum) {
        if (enumName == null) {
            return defaultEnum;
        }
        try {
            return Enum.valueOf(enumClass, enumName);
        } catch (final IllegalArgumentException ex) {
            return defaultEnum;
        }
    }

    /**
     * Gets the enum for the class, returning {@code null} if not found.
     *
     * <p>This method differs from {@link Enum#valueOf} in that it does not throw an exception
     * for an invalid enum name and performs case insensitive matching of the name.</p>
     *
     * @param <E>         the type of the enumeration
     * @param enumClass   the class of the enum to query, not null
     * @param enumName    the enum name, null returns null
     * @return the enum, null if not found
     * @since 3.8
     */
    public static <E extends Enum<E>> E getEnumIgnoreCase(final Class<E> enumClass, final String enumName) {
        return getEnumIgnoreCase(enumClass, enumName, null);
    }

    /**
     * Gets the enum for the class, returning {@code defaultEnum} if not found.
     *
     * <p>This method differs from {@link Enum#valueOf} in that it does not throw an exception
     * for an invalid enum name and performs case insensitive matching of the name.</p>
     *
     * @param <E>         the type of the enumeration
     * @param enumClass   the class of the enum to query, not null
     * @param enumName    the enum name, null returns default enum
     * @param defaultEnum the default enum
     * @return the enum, default enum if not found
     * @since 3.10
     */
    public static <E extends Enum<E>> E getEnumIgnoreCase(final Class<E> enumClass, final String enumName,
        final E defaultEnum) {
        return getFirstEnumIgnoreCase(enumClass, enumName, Enum::name, defaultEnum);
    }

    /**
     * Gets the {@link List} of enums.
     *
     * <p>This method is useful when you need a list of enums rather than an array.</p>
     *
     * @param <E> the type of the enumeration
     * @param enumClass  the class of the enum to query, not null
     * @return the modifiable list of enums, never null
     */
    public static <E extends Enum<E>> List<E> getEnumList(final Class<E> enumClass) {
        return new ArrayList<>(Arrays.asList(enumClass.getEnumConstants()));
    }

    /**
     * Gets the {@link Map} of enums by name.
     *
     * <p>This method is useful when you need a map of enums by name.</p>
     *
     * @param <E> the type of the enumeration
     * @param enumClass  the class of the enum to query, not null
     * @return the modifiable map of enum names to enums, never null
     */
    public static <E extends Enum<E>> Map<String, E> getEnumMap(final Class<E> enumClass) {
        return getEnumMap(enumClass, E::name);
    }

    /**
     * Gets the {@link Map} of enums by name.
     *
     * <p>
     * This method is useful when you need a map of enums by name.
     * </p>
     *
     * @param <E>         the type of enumeration
     * @param <K>         the type of the map key
     * @param enumClass   the class of the enum to query, not null
     * @param keyFunction the function to query for the key, not null
     * @return the modifiable map of enums, never null
     * @since 3.13.0
     */
    public static <E extends Enum<E>, K> Map<K, E> getEnumMap(final Class<E> enumClass, final Function<E, K> keyFunction) {
        return Stream.of(enumClass.getEnumConstants()).collect(Collectors.toMap(keyFunction::apply, Function.identity()));
    }

    /**
     * Gets the enum for the class in a system property, returning {@code defaultEnum} if not found.
     *
     * <p>
     * This method differs from {@link Enum#valueOf} in that it does not throw an exception for an invalid enum name.
     * </p>
     *
     * @param <E> the type of the enumeration
     * @param enumClass the class of the enum to query, not null
     * @param propName the system property key for the enum name, null returns default enum
     * @param defaultEnum the default enum
     * @return the enum, default enum if not found
     * @since 3.13.0
     */
    public static <E extends Enum<E>> E getEnumSystemProperty(final Class<E> enumClass, final String propName,
        final E defaultEnum) {
        return enumClass == null || propName == null ? defaultEnum
            : getEnum(enumClass, System.getProperty(propName), defaultEnum);
    }

    /**
     * Gets the enum for the class, returning {@code defaultEnum} if not found.
     *
     * <p>This method differs from {@link Enum#valueOf} in that it does not throw an exception
     * for an invalid enum name and performs case insensitive matching of the name.</p>
     *
     * @param <E>         the type of the enumeration
     * @param enumClass   the class of the enum to query, not null
     * @param enumName    the enum name, null returns default enum
     * @param stringFunction the function that gets the string for an enum for comparison to {@code enumName}.
     * @param defaultEnum the default enum
     * @return the enum, default enum if not found
     * @since 3.13.0
     */
    public static <E extends Enum<E>> E getFirstEnumIgnoreCase(final Class<E> enumClass, final String enumName, final Function<E, String> stringFunction,
        final E defaultEnum) {
        if (enumName == null || !enumClass.isEnum()) {
            return defaultEnum;
        }
        return Stream.of(enumClass.getEnumConstants()).filter(e -> enumName.equalsIgnoreCase(stringFunction.apply(e))).findFirst().orElse(defaultEnum);
    }

    /**
     * Checks if the specified name is a valid enum for the class.
     *
     * <p>This method differs from {@link Enum#valueOf} in that checks if the name is
     * a valid enum without needing to catch the exception.</p>
     *
     * @param <E> the type of the enumeration
     * @param enumClass  the class of the enum to query, not null
     * @param enumName   the enum name, null returns false
     * @return true if the enum name is valid, otherwise false
     */
    public static <E extends Enum<E>> boolean isValidEnum(final Class<E> enumClass, final String enumName) {
        return getEnum(enumClass, enumName) != null;
    }

    /**
     * Checks if the specified name is a valid enum for the class.
     *
     * <p>This method differs from {@link Enum#valueOf} in that checks if the name is
     * a valid enum without needing to catch the exception
     * and performs case insensitive matching of the name.</p>
     *
     * @param <E> the type of the enumeration
     * @param enumClass  the class of the enum to query, not null
     * @param enumName   the enum name, null returns false
     * @return true if the enum name is valid, otherwise false
     * @since 3.8
     */
    public static <E extends Enum<E>> boolean isValidEnumIgnoreCase(final Class<E> enumClass, final String enumName) {
        return getEnumIgnoreCase(enumClass, enumName) != null;
    }

    /**
     * Convert a long value created by {@link EnumUtils#generateBitVector} into the set of
     * enum values that it represents.
     *
     * <p>If you store this value, beware any changes to the enum that would affect ordinal values.</p>
     * @param enumClass the class of the enum we are working with, not {@code null}
     * @param value     the long value representation of a set of enum values
     * @param <E>       the type of the enumeration
     * @return a set of enum values
     * @throws NullPointerException if {@code enumClass} is {@code null}
     * @throws IllegalArgumentException if {@code enumClass} is not an enum class or has more than 64 values
     * @since 3.0.1
     */
    public static <E extends Enum<E>> EnumSet<E> processBitVector(final Class<E> enumClass, final long value) {
        checkBitVectorable(enumClass).getEnumConstants();
        return processBitVectors(enumClass, value);
    }

    /**
     * Convert a {@code long[]} created by {@link EnumUtils#generateBitVectors} into the set of
     * enum values that it represents.
     *
     * <p>If you store this value, beware any changes to the enum that would affect ordinal values.</p>
     * @param enumClass the class of the enum we are working with, not {@code null}
     * @param values     the long[] bearing the representation of a set of enum values, the least significant digits rightmost, not {@code null}
     * @param <E>       the type of the enumeration
     * @return a set of enum values
     * @throws NullPointerException if {@code enumClass} is {@code null}
     * @throws IllegalArgumentException if {@code enumClass} is not an enum class
     * @since 3.2
     */
    public static <E extends Enum<E>> EnumSet<E> processBitVectors(final Class<E> enumClass, final long... values) {
        final EnumSet<E> results = EnumSet.noneOf(asEnum(enumClass));
        final long[] lvalues = ArrayUtils.clone(Objects.requireNonNull(values, "values"));
        ArrayUtils.reverse(lvalues);
        for (final E constant : enumClass.getEnumConstants()) {
            final int block = constant.ordinal() / Long.SIZE;
            if (block < lvalues.length && (lvalues[block] & 1L << (constant.ordinal() % Long.SIZE)) != 0) {
                results.add(constant);
            }
        }
        return results;
    }

    /**
     * This constructor is public to permit tools that require a JavaBean
     * instance to operate.
     */
    public EnumUtils() {
    }
}
