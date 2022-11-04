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

package org.apache.commons.lang3.builder;

import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.lang3.ArraySorter;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.Validate;

/**
 * Assists in implementing {@link Object#hashCode()} methods.
 *
 * <p>
 * This class enables a good {@code hashCode} method to be built for any class. It follows the rules laid out in
 * the book <a href="https://www.oracle.com/technetwork/java/effectivejava-136174.html">Effective Java</a> by Joshua Bloch. Writing a
 * good {@code hashCode} method is actually quite difficult. This class aims to simplify the process.
 * </p>
 *
 * <p>
 * The following is the approach taken. When appending a data field, the current total is multiplied by the
 * multiplier then a relevant value
 * for that data type is added. For example, if the current hashCode is 17, and the multiplier is 37, then
 * appending the integer 45 will create a hash code of 674, namely 17 * 37 + 45.
 * </p>
 *
 * <p>
 * All relevant fields from the object should be included in the {@code hashCode} method. Derived fields may be
 * excluded. In general, any field used in the {@code equals} method must be used in the {@code hashCode}
 * method.
 * </p>
 *
 * <p>
 * To use this class write code as follows:
 * </p>
 *
 * <pre>
 * public class Person {
 *   String name;
 *   int age;
 *   boolean smoker;
 *   ...
 *
 *   public int hashCode() {
 *     // you pick a hard-coded, randomly chosen, non-zero, odd number
 *     // ideally different for each class
 *     return new HashCodeBuilder(17, 37).
 *       append(name).
 *       append(age).
 *       append(smoker).
 *       toHashCode();
 *   }
 * }
 * </pre>
 *
 * <p>
 * If required, the superclass {@code hashCode()} can be added using {@link #appendSuper}.
 * </p>
 *
 * <p>
 * Alternatively, there is a method that uses reflection to determine the fields to test. Because these fields are
 * usually private, the method, {@code reflectionHashCode}, uses {@code AccessibleObject.setAccessible}
 * to change the visibility of the fields. This will fail under a security manager, unless the appropriate permissions
 * are set up correctly. It is also slower than testing explicitly.
 * </p>
 *
 * <p>
 * A typical invocation for this method would look like:
 * </p>
 *
 * <pre>
 * public int hashCode() {
 *   return HashCodeBuilder.reflectionHashCode(this);
 * }
 * </pre>
 *
 * <p>The {@link HashCodeExclude} annotation can be used to exclude fields from being
 * used by the {@code reflectionHashCode} methods.</p>
 *
 * @since 1.0
 */
public class HashCodeBuilder implements Builder<Integer> {
    /**
     * The default initial value to use in reflection hash code building.
     */
    private static final int DEFAULT_INITIAL_VALUE = 17;

    /**
     * The default multiplier value to use in reflection hash code building.
     */
    private static final int DEFAULT_MULTIPLIER_VALUE = 37;

    /**
     * A registry of objects used by reflection methods to detect cyclical object references and avoid infinite loops.
     *
     * @since 2.3
     */
    private static final ThreadLocal<Set<IDKey>> REGISTRY = new ThreadLocal<>();

    /*
     * NOTE: we cannot store the actual objects in a HashSet, as that would use the very hashCode()
     * we are in the process of calculating.
     *
     * So we generate a one-to-one mapping from the original object to a new object.
     *
     * Now HashSet uses equals() to determine if two elements with the same hash code really
     * are equal, so we also need to ensure that the replacement objects are only equal
     * if the original objects are identical.
     *
     * The original implementation (2.4 and before) used the System.identityHashCode()
     * method - however this is not guaranteed to generate unique ids (e.g. LANG-459)
     *
     * We now use the IDKey helper class (adapted from org.apache.axis.utils.IDKey)
     * to disambiguate the duplicate ids.
     */

    /**
     * Returns the registry of objects being traversed by the reflection methods in the current thread.
     *
     * @return Set the registry of objects being traversed
     * @since 2.3
     */
    static Set<IDKey> getRegistry() {
        return REGISTRY.get();
    }

    /**
     * Returns {@code true} if the registry contains the given object. Used by the reflection methods to avoid
     * infinite loops.
     *
     * @param value
     *            The object to lookup in the registry.
     * @return boolean {@code true} if the registry contains the given object.
     * @since 2.3
     */
    static boolean isRegistered(final Object value) {
        final Set<IDKey> registry = getRegistry();
        return registry != null && registry.contains(new IDKey(value));
    }

    /**
     * Appends the fields and values defined by the given object of the given {@link Class}.
     *
     * @param object
     *            the object to append details of
     * @param clazz
     *            the class to append details of
     * @param builder
     *            the builder to append to
     * @param useTransients
     *            whether to use transient fields
     * @param excludeFields
     *            Collection of String field names to exclude from use in calculation of hash code
     */
    private static void reflectionAppend(final Object object, final Class<?> clazz, final HashCodeBuilder builder, final boolean useTransients,
            final String[] excludeFields) {
        if (isRegistered(object)) {
            return;
        }
        try {
            register(object);
            // The elements in the returned array are not sorted and are not in any particular order.
            final Field[] fields = ArraySorter.sort(clazz.getDeclaredFields(), Comparator.comparing(Field::getName));
            AccessibleObject.setAccessible(fields, true);
            for (final Field field : fields) {
                if (!ArrayUtils.contains(excludeFields, field.getName())
                    && !field.getName().contains("$")
                    && (useTransients || !Modifier.isTransient(field.getModifiers()))
                    && !Modifier.isStatic(field.getModifiers())
                    && !field.isAnnotationPresent(HashCodeExclude.class)) {
                    try {
                        final Object fieldValue = field.get(object);
                        builder.append(fieldValue);
                    } catch (final IllegalAccessException e) {
                        // this can't happen. Would get a Security exception instead
                        // throw a runtime exception in case the impossible happens.
                        throw new InternalError("Unexpected IllegalAccessException");
                    }
                }
            }
        } finally {
            unregister(object);
        }
    }

    /**
     * Uses reflection to build a valid hash code from the fields of {@code object}.
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * Transient members will be not be used, as they are likely derived fields, and not part of the value of the
     * {@link Object}.
     * </p>
     *
     * <p>
     * Static fields will not be tested. Superclass fields will be included.
     * </p>
     *
     * <p>
     * Two randomly chosen, non-zero, odd numbers must be passed in. Ideally these should be different for each class,
     * however this is not vital. Prime numbers are preferred, especially for the multiplier.
     * </p>
     *
     * @param initialNonZeroOddNumber
     *            a non-zero, odd number used as the initial value. This will be the returned
     *            value if no fields are found to include in the hash code
     * @param multiplierNonZeroOddNumber
     *            a non-zero, odd number used as the multiplier
     * @param object
     *            the Object to create a {@code hashCode} for
     * @return int hash code
     * @throws IllegalArgumentException
     *             if the Object is {@code null}
     * @throws IllegalArgumentException
     *             if the number is zero or even
     *
     * @see HashCodeExclude
     */
    public static int reflectionHashCode(final int initialNonZeroOddNumber, final int multiplierNonZeroOddNumber, final Object object) {
        return reflectionHashCode(initialNonZeroOddNumber, multiplierNonZeroOddNumber, object, false, null);
    }

    /**
     * Uses reflection to build a valid hash code from the fields of {@code object}.
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * If the TestTransients parameter is set to {@code true}, transient members will be tested, otherwise they
     * are ignored, as they are likely derived fields, and not part of the value of the {@link Object}.
     * </p>
     *
     * <p>
     * Static fields will not be tested. Superclass fields will be included.
     * </p>
     *
     * <p>
     * Two randomly chosen, non-zero, odd numbers must be passed in. Ideally these should be different for each class,
     * however this is not vital. Prime numbers are preferred, especially for the multiplier.
     * </p>
     *
     * @param initialNonZeroOddNumber
     *            a non-zero, odd number used as the initial value. This will be the returned
     *            value if no fields are found to include in the hash code
     * @param multiplierNonZeroOddNumber
     *            a non-zero, odd number used as the multiplier
     * @param object
     *            the Object to create a {@code hashCode} for
     * @param testTransients
     *            whether to include transient fields
     * @return int hash code
     * @throws IllegalArgumentException
     *             if the Object is {@code null}
     * @throws IllegalArgumentException
     *             if the number is zero or even
     *
     * @see HashCodeExclude
     */
    public static int reflectionHashCode(final int initialNonZeroOddNumber, final int multiplierNonZeroOddNumber, final Object object,
            final boolean testTransients) {
        return reflectionHashCode(initialNonZeroOddNumber, multiplierNonZeroOddNumber, object, testTransients, null);
    }

    /**
     * Uses reflection to build a valid hash code from the fields of {@code object}.
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * If the TestTransients parameter is set to {@code true}, transient members will be tested, otherwise they
     * are ignored, as they are likely derived fields, and not part of the value of the {@link Object}.
     * </p>
     *
     * <p>
     * Static fields will not be included. Superclass fields will be included up to and including the specified
     * superclass. A null superclass is treated as java.lang.Object.
     * </p>
     *
     * <p>
     * Two randomly chosen, non-zero, odd numbers must be passed in. Ideally these should be different for each class,
     * however this is not vital. Prime numbers are preferred, especially for the multiplier.
     * </p>
     *
     * @param <T>
     *            the type of the object involved
     * @param initialNonZeroOddNumber
     *            a non-zero, odd number used as the initial value. This will be the returned
     *            value if no fields are found to include in the hash code
     * @param multiplierNonZeroOddNumber
     *            a non-zero, odd number used as the multiplier
     * @param object
     *            the Object to create a {@code hashCode} for
     * @param testTransients
     *            whether to include transient fields
     * @param reflectUpToClass
     *            the superclass to reflect up to (inclusive), may be {@code null}
     * @param excludeFields
     *            array of field names to exclude from use in calculation of hash code
     * @return int hash code
     * @throws IllegalArgumentException
     *             if the Object is {@code null}
     * @throws IllegalArgumentException
     *             if the number is zero or even
     *
     * @see HashCodeExclude
     * @since 2.0
     */
    public static <T> int reflectionHashCode(final int initialNonZeroOddNumber, final int multiplierNonZeroOddNumber, final T object,
            final boolean testTransients, final Class<? super T> reflectUpToClass, final String... excludeFields) {
        Objects.requireNonNull(object, "object");
        final HashCodeBuilder builder = new HashCodeBuilder(initialNonZeroOddNumber, multiplierNonZeroOddNumber);
        Class<?> clazz = object.getClass();
        reflectionAppend(object, clazz, builder, testTransients, excludeFields);
        while (clazz.getSuperclass() != null && clazz != reflectUpToClass) {
            clazz = clazz.getSuperclass();
            reflectionAppend(object, clazz, builder, testTransients, excludeFields);
        }
        return builder.toHashCode();
    }

    /**
     * Uses reflection to build a valid hash code from the fields of {@code object}.
     *
     * <p>
     * This constructor uses two hard coded choices for the constants needed to build a hash code.
     * </p>
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * If the TestTransients parameter is set to {@code true}, transient members will be tested, otherwise they
     * are ignored, as they are likely derived fields, and not part of the value of the {@link Object}.
     * </p>
     *
     * <p>
     * Static fields will not be tested. Superclass fields will be included. If no fields are found to include
     * in the hash code, the result of this method will be constant.
     * </p>
     *
     * @param object
     *            the Object to create a {@code hashCode} for
     * @param testTransients
     *            whether to include transient fields
     * @return int hash code
     * @throws IllegalArgumentException
     *             if the object is {@code null}
     *
     * @see HashCodeExclude
     */
    public static int reflectionHashCode(final Object object, final boolean testTransients) {
        return reflectionHashCode(DEFAULT_INITIAL_VALUE, DEFAULT_MULTIPLIER_VALUE, object,
                testTransients, null);
    }

    /**
     * Uses reflection to build a valid hash code from the fields of {@code object}.
     *
     * <p>
     * This constructor uses two hard coded choices for the constants needed to build a hash code.
     * </p>
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * Transient members will be not be used, as they are likely derived fields, and not part of the value of the
     * {@link Object}.
     * </p>
     *
     * <p>
     * Static fields will not be tested. Superclass fields will be included. If no fields are found to include
     * in the hash code, the result of this method will be constant.
     * </p>
     *
     * @param object
     *            the Object to create a {@code hashCode} for
     * @param excludeFields
     *            Collection of String field names to exclude from use in calculation of hash code
     * @return int hash code
     * @throws IllegalArgumentException
     *             if the object is {@code null}
     *
     * @see HashCodeExclude
     */
    public static int reflectionHashCode(final Object object, final Collection<String> excludeFields) {
        return reflectionHashCode(object, ReflectionToStringBuilder.toNoNullStringArray(excludeFields));
    }

    /**
     * Uses reflection to build a valid hash code from the fields of {@code object}.
     *
     * <p>
     * This constructor uses two hard coded choices for the constants needed to build a hash code.
     * </p>
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * Transient members will be not be used, as they are likely derived fields, and not part of the value of the
     * {@link Object}.
     * </p>
     *
     * <p>
     * Static fields will not be tested. Superclass fields will be included. If no fields are found to include
     * in the hash code, the result of this method will be constant.
     * </p>
     *
     * @param object
     *            the Object to create a {@code hashCode} for
     * @param excludeFields
     *            array of field names to exclude from use in calculation of hash code
     * @return int hash code
     * @throws IllegalArgumentException
     *             if the object is {@code null}
     *
     * @see HashCodeExclude
     */
    public static int reflectionHashCode(final Object object, final String... excludeFields) {
        return reflectionHashCode(DEFAULT_INITIAL_VALUE, DEFAULT_MULTIPLIER_VALUE, object, false,
                null, excludeFields);
    }

    /**
     * Registers the given object. Used by the reflection methods to avoid infinite loops.
     *
     * @param value
     *            The object to register.
     */
    private static void register(final Object value) {
        Set<IDKey> registry = getRegistry();
        if (registry == null) {
            registry = new HashSet<>();
            REGISTRY.set(registry);
        }
        registry.add(new IDKey(value));
    }

    /**
     * Unregisters the given object.
     *
     * <p>
     * Used by the reflection methods to avoid infinite loops.
     *
     * @param value
     *            The object to unregister.
     * @since 2.3
     */
    private static void unregister(final Object value) {
        final Set<IDKey> registry = getRegistry();
        if (registry != null) {
            registry.remove(new IDKey(value));
            if (registry.isEmpty()) {
                REGISTRY.remove();
            }
        }
    }

    /**
     * Constant to use in building the hashCode.
     */
    private final int iConstant;

    /**
     * Running total of the hashCode.
     */
    private int iTotal;

    /**
     * Uses two hard coded choices for the constants needed to build a {@code hashCode}.
     *
     */
    public HashCodeBuilder() {
        iConstant = 37;
        iTotal = 17;
    }

    /**
     * Two randomly chosen, odd numbers must be passed in. Ideally these should be different for each class,
     * however this is not vital.
     *
     * <p>
     * Prime numbers are preferred, especially for the multiplier.
     * </p>
     *
     * @param initialOddNumber
     *            an odd number used as the initial value
     * @param multiplierOddNumber
     *            an odd number used as the multiplier
     * @throws IllegalArgumentException
     *             if the number is even
     */
    public HashCodeBuilder(final int initialOddNumber, final int multiplierOddNumber) {
        Validate.isTrue(initialOddNumber % 2 != 0, "HashCodeBuilder requires an odd initial value");
        Validate.isTrue(multiplierOddNumber % 2 != 0, "HashCodeBuilder requires an odd multiplier");
        iConstant = multiplierOddNumber;
        iTotal = initialOddNumber;
    }

    /**
     * Append a {@code hashCode} for a {@code boolean}.
     *
     * <p>
     * This adds {@code 1} when true, and {@code 0} when false to the {@code hashCode}.
     * </p>
     * <p>
     * This is in contrast to the standard {@code java.lang.Boolean.hashCode} handling, which computes
     * a {@code hashCode} value of {@code 1231} for {@code java.lang.Boolean} instances
     * that represent {@code true} or {@code 1237} for {@code java.lang.Boolean} instances
     * that represent {@code false}.
     * </p>
     * <p>
     * This is in accordance with the <i>Effective Java</i> design.
     * </p>
     *
     * @param value
     *            the boolean to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final boolean value) {
        iTotal = iTotal * iConstant + (value ? 0 : 1);
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code boolean} array.
     *
     * @param array
     *            the array to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final boolean[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (final boolean element : array) {
                append(element);
            }
        }
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code byte}.
     *
     * @param value
     *            the byte to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final byte value) {
        iTotal = iTotal * iConstant + value;
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code byte} array.
     *
     * @param array
     *            the array to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final byte[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (final byte element : array) {
                append(element);
            }
        }
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code char}.
     *
     * @param value
     *            the char to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final char value) {
        iTotal = iTotal * iConstant + value;
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code char} array.
     *
     * @param array
     *            the array to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final char[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (final char element : array) {
                append(element);
            }
        }
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code double}.
     *
     * @param value
     *            the double to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final double value) {
        return append(Double.doubleToLongBits(value));
    }

    /**
     * Append a {@code hashCode} for a {@code double} array.
     *
     * @param array
     *            the array to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final double[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (final double element : array) {
                append(element);
            }
        }
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code float}.
     *
     * @param value
     *            the float to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final float value) {
        iTotal = iTotal * iConstant + Float.floatToIntBits(value);
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code float} array.
     *
     * @param array
     *            the array to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final float[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (final float element : array) {
                append(element);
            }
        }
        return this;
    }

    /**
     * Append a {@code hashCode} for an {@code int}.
     *
     * @param value
     *            the int to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final int value) {
        iTotal = iTotal * iConstant + value;
        return this;
    }

    /**
     * Append a {@code hashCode} for an {@code int} array.
     *
     * @param array
     *            the array to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final int[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (final int element : array) {
                append(element);
            }
        }
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code long}.
     *
     * @param value
     *            the long to add to the {@code hashCode}
     * @return this
     */
    // NOTE: This method uses >> and not >>> as Effective Java and
    //       Long.hashCode do. Ideally we should switch to >>> at
    //       some stage. There are backwards compat issues, so
    //       that will have to wait for the time being. cf LANG-342.
    public HashCodeBuilder append(final long value) {
        iTotal = iTotal * iConstant + (int) (value ^ value >> 32);
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code long} array.
     *
     * @param array
     *            the array to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final long[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (final long element : array) {
                append(element);
            }
        }
        return this;
    }

    /**
     * Append a {@code hashCode} for an {@link Object}.
     *
     * @param object
     *            the Object to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final Object object) {
        if (object == null) {
            iTotal = iTotal * iConstant;

        } else if (ObjectUtils.isArray(object)) {
            // factor out array case in order to keep method small enough
            // to be inlined
            appendArray(object);
        } else {
            iTotal = iTotal * iConstant + object.hashCode();
        }
        return this;
    }

    /**
     * Append a {@code hashCode} for an {@link Object} array.
     *
     * @param array
     *            the array to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final Object[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (final Object element : array) {
                append(element);
            }
        }
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code short}.
     *
     * @param value
     *            the short to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final short value) {
        iTotal = iTotal * iConstant + value;
        return this;
    }

    /**
     * Append a {@code hashCode} for a {@code short} array.
     *
     * @param array
     *            the array to add to the {@code hashCode}
     * @return this
     */
    public HashCodeBuilder append(final short[] array) {
        if (array == null) {
            iTotal = iTotal * iConstant;
        } else {
            for (final short element : array) {
                append(element);
            }
        }
        return this;
    }

    /**
     * Append a {@code hashCode} for an array.
     *
     * @param object
     *            the array to add to the {@code hashCode}
     */
    private void appendArray(final Object object) {
        // 'Switch' on type of array, to dispatch to the correct handler
        // This handles multidimensional arrays
        if (object instanceof long[]) {
            append((long[]) object);
        } else if (object instanceof int[]) {
            append((int[]) object);
        } else if (object instanceof short[]) {
            append((short[]) object);
        } else if (object instanceof char[]) {
            append((char[]) object);
        } else if (object instanceof byte[]) {
            append((byte[]) object);
        } else if (object instanceof double[]) {
            append((double[]) object);
        } else if (object instanceof float[]) {
            append((float[]) object);
        } else if (object instanceof boolean[]) {
            append((boolean[]) object);
        } else {
            // Not an array of primitives
            append((Object[]) object);
        }
    }

    /**
     * Adds the result of super.hashCode() to this builder.
     *
     * @param superHashCode
     *            the result of calling {@code super.hashCode()}
     * @return this HashCodeBuilder, used to chain calls.
     * @since 2.0
     */
    public HashCodeBuilder appendSuper(final int superHashCode) {
        iTotal = iTotal * iConstant + superHashCode;
        return this;
    }

    /**
     * Returns the computed {@code hashCode}.
     *
     * @return {@code hashCode} based on the fields appended
     *
     * @since 3.0
     */
    @Override
    public Integer build() {
        return Integer.valueOf(toHashCode());
    }

    /**
     * Implements equals using the hash code.
     *
     * @since 3.13.0
     */
    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof HashCodeBuilder)) {
            return false;
        }
        final HashCodeBuilder other = (HashCodeBuilder) obj;
        return iTotal == other.iTotal;
    }

    /**
     * The computed {@code hashCode} from toHashCode() is returned due to the likelihood
     * of bugs in mis-calling toHashCode() and the unlikeliness of it mattering what the hashCode for
     * HashCodeBuilder itself is.
     *
     * @return {@code hashCode} based on the fields appended
     * @since 2.5
     */
    @Override
    public int hashCode() {
        return toHashCode();
    }

    /**
     * Returns the computed {@code hashCode}.
     *
     * @return {@code hashCode} based on the fields appended
     */
    public int toHashCode() {
        return iTotal;
    }

}
