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
import java.util.Objects;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ObjectUtils;

/**
 * Assists in implementing {@link java.lang.Comparable#compareTo(Object)} methods.
 *
 * <p>It is consistent with {@code equals(Object)} and
 * {@code hashcode()} built with {@link EqualsBuilder} and
 * {@link HashCodeBuilder}.</p>
 *
 * <p>Two Objects that compare equal using {@code equals(Object)} should normally
 * also compare equal using {@code compareTo(Object)}.</p>
 *
 * <p>All relevant fields should be included in the calculation of the
 * comparison. Derived fields may be ignored. The same fields, in the same
 * order, should be used in both {@code compareTo(Object)} and
 * {@code equals(Object)}.</p>
 *
 * <p>To use this class write code as follows:</p>
 *
 * <pre>
 * public class MyClass {
 *   String field1;
 *   int field2;
 *   boolean field3;
 *
 *   ...
 *
 *   public int compareTo(Object o) {
 *     MyClass myClass = (MyClass) o;
 *     return new CompareToBuilder()
 *       .appendSuper(super.compareTo(o)
 *       .append(this.field1, myClass.field1)
 *       .append(this.field2, myClass.field2)
 *       .append(this.field3, myClass.field3)
 *       .toComparison();
 *   }
 * }
 * </pre>
 *
 * <p>Values are compared in the order they are appended to the builder. If any comparison returns
 * a non-zero result, then that value will be the result returned by {@code toComparison()} and all
 * subsequent comparisons are skipped.</p>
 *
 * <p>Alternatively, there are {@link #reflectionCompare(Object, Object) reflectionCompare} methods that use
 * reflection to determine the fields to append. Because fields can be private,
 * {@code reflectionCompare} uses {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} to
 * bypass normal access control checks. This will fail under a security manager,
 * unless the appropriate permissions are set up correctly. It is also
 * slower than appending explicitly.</p>
 *
 * <p>A typical implementation of {@code compareTo(Object)} using
 * {@code reflectionCompare} looks like:</p>

 * <pre>
 * public int compareTo(Object o) {
 *   return CompareToBuilder.reflectionCompare(this, o);
 * }
 * </pre>
 *
 * <p>The reflective methods compare object fields in the order returned by
 * {@link Class#getDeclaredFields()}. The fields of the class are compared first, followed by those
 * of its parent classes (in order from the bottom to the top of the class hierarchy).</p>
 *
 * @see Comparable
 * @see Object#equals(Object)
 * @see Object#hashCode()
 * @see EqualsBuilder
 * @see HashCodeBuilder
 * @since 1.0
 */
public class CompareToBuilder implements Builder<Integer> {

    /**
     * Current state of the comparison as appended fields are checked.
     */
    private int comparison;

    /**
     * Constructor for CompareToBuilder.
     *
     * <p>Starts off assuming that the objects are equal. Multiple calls are
     * then made to the various append methods, followed by a call to
     * {@link #toComparison} to get the result.</p>
     */
    public CompareToBuilder() {
        comparison = 0;
    }

    /**
     * Compares two {@link Object}s via reflection.
     *
     * <p>Fields can be private, thus {@code AccessibleObject.setAccessible}
     * is used to bypass normal access control checks. This will fail under a
     * security manager unless the appropriate permissions are set.</p>
     *
     * <ul>
     * <li>Static fields will not be compared</li>
     * <li>Transient members will be not be compared, as they are likely derived
     *     fields</li>
     * <li>Superclass fields will be compared</li>
     * </ul>
     *
     * <p>If both {@code lhs} and {@code rhs} are {@code null},
     * they are considered equal.</p>
     *
     * @param lhs  left-hand object
     * @param rhs  right-hand object
     * @return a negative integer, zero, or a positive integer as {@code lhs}
     *  is less than, equal to, or greater than {@code rhs}
     * @throws NullPointerException  if either (but not both) parameters are
     *  {@code null}
     * @throws ClassCastException  if {@code rhs} is not assignment-compatible
     *  with {@code lhs}
     */
    public static int reflectionCompare(final Object lhs, final Object rhs) {
        return reflectionCompare(lhs, rhs, false, null);
    }

    /**
     * Compares two {@link Object}s via reflection.
     *
     * <p>Fields can be private, thus {@code AccessibleObject.setAccessible}
     * is used to bypass normal access control checks. This will fail under a
     * security manager unless the appropriate permissions are set.</p>
     *
     * <ul>
     * <li>Static fields will not be compared</li>
     * <li>If {@code compareTransients} is {@code true},
     *     compares transient members.  Otherwise ignores them, as they
     *     are likely derived fields.</li>
     * <li>Superclass fields will be compared</li>
     * </ul>
     *
     * <p>If both {@code lhs} and {@code rhs} are {@code null},
     * they are considered equal.</p>
     *
     * @param lhs  left-hand object
     * @param rhs  right-hand object
     * @param compareTransients  whether to compare transient fields
     * @return a negative integer, zero, or a positive integer as {@code lhs}
     *  is less than, equal to, or greater than {@code rhs}
     * @throws NullPointerException  if either {@code lhs} or {@code rhs}
     *  (but not both) is {@code null}
     * @throws ClassCastException  if {@code rhs} is not assignment-compatible
     *  with {@code lhs}
     */
    public static int reflectionCompare(final Object lhs, final Object rhs, final boolean compareTransients) {
        return reflectionCompare(lhs, rhs, compareTransients, null);
    }

    /**
     * Compares two {@link Object}s via reflection.
     *
     * <p>Fields can be private, thus {@code AccessibleObject.setAccessible}
     * is used to bypass normal access control checks. This will fail under a
     * security manager unless the appropriate permissions are set.</p>
     *
     * <ul>
     * <li>Static fields will not be compared</li>
     * <li>If {@code compareTransients} is {@code true},
     *     compares transient members.  Otherwise ignores them, as they
     *     are likely derived fields.</li>
     * <li>Superclass fields will be compared</li>
     * </ul>
     *
     * <p>If both {@code lhs} and {@code rhs} are {@code null},
     * they are considered equal.</p>
     *
     * @param lhs  left-hand object
     * @param rhs  right-hand object
     * @param excludeFields  Collection of String fields to exclude
     * @return a negative integer, zero, or a positive integer as {@code lhs}
     *  is less than, equal to, or greater than {@code rhs}
     * @throws NullPointerException  if either {@code lhs} or {@code rhs}
     *  (but not both) is {@code null}
     * @throws ClassCastException  if {@code rhs} is not assignment-compatible
     *  with {@code lhs}
     * @since 2.2
     */
    public static int reflectionCompare(final Object lhs, final Object rhs, final Collection<String> excludeFields) {
        return reflectionCompare(lhs, rhs, ReflectionToStringBuilder.toNoNullStringArray(excludeFields));
    }

    /**
     * Compares two {@link Object}s via reflection.
     *
     * <p>Fields can be private, thus {@code AccessibleObject.setAccessible}
     * is used to bypass normal access control checks. This will fail under a
     * security manager unless the appropriate permissions are set.</p>
     *
     * <ul>
     * <li>Static fields will not be compared</li>
     * <li>If {@code compareTransients} is {@code true},
     *     compares transient members.  Otherwise ignores them, as they
     *     are likely derived fields.</li>
     * <li>Superclass fields will be compared</li>
     * </ul>
     *
     * <p>If both {@code lhs} and {@code rhs} are {@code null},
     * they are considered equal.</p>
     *
     * @param lhs  left-hand object
     * @param rhs  right-hand object
     * @param excludeFields  array of fields to exclude
     * @return a negative integer, zero, or a positive integer as {@code lhs}
     *  is less than, equal to, or greater than {@code rhs}
     * @throws NullPointerException  if either {@code lhs} or {@code rhs}
     *  (but not both) is {@code null}
     * @throws ClassCastException  if {@code rhs} is not assignment-compatible
     *  with {@code lhs}
     * @since 2.2
     */
    public static int reflectionCompare(final Object lhs, final Object rhs, final String... excludeFields) {
        return reflectionCompare(lhs, rhs, false, null, excludeFields);
    }

    /**
     * Compares two {@link Object}s via reflection.
     *
     * <p>Fields can be private, thus {@code AccessibleObject.setAccessible}
     * is used to bypass normal access control checks. This will fail under a
     * security manager unless the appropriate permissions are set.</p>
     *
     * <ul>
     * <li>Static fields will not be compared</li>
     * <li>If the {@code compareTransients} is {@code true},
     *     compares transient members.  Otherwise ignores them, as they
     *     are likely derived fields.</li>
     * <li>Compares superclass fields up to and including {@code reflectUpToClass}.
     *     If {@code reflectUpToClass} is {@code null}, compares all superclass fields.</li>
     * </ul>
     *
     * <p>If both {@code lhs} and {@code rhs} are {@code null},
     * they are considered equal.</p>
     *
     * @param lhs  left-hand object
     * @param rhs  right-hand object
     * @param compareTransients  whether to compare transient fields
     * @param reflectUpToClass  last superclass for which fields are compared
     * @param excludeFields  fields to exclude
     * @return a negative integer, zero, or a positive integer as {@code lhs}
     *  is less than, equal to, or greater than {@code rhs}
     * @throws NullPointerException  if either {@code lhs} or {@code rhs}
     *  (but not both) is {@code null}
     * @throws ClassCastException  if {@code rhs} is not assignment-compatible
     *  with {@code lhs}
     * @since 2.2 (2.0 as {@code reflectionCompare(Object, Object, boolean, Class)})
     */
    public static int reflectionCompare(
        final Object lhs,
        final Object rhs,
        final boolean compareTransients,
        final Class<?> reflectUpToClass,
        final String... excludeFields) {

        if (lhs == rhs) {
            return 0;
        }
        Objects.requireNonNull(lhs, "lhs");
        Objects.requireNonNull(rhs, "rhs");

        Class<?> lhsClazz = lhs.getClass();
        if (!lhsClazz.isInstance(rhs)) {
            throw new ClassCastException();
        }
        final CompareToBuilder compareToBuilder = new CompareToBuilder();
        reflectionAppend(lhs, rhs, lhsClazz, compareToBuilder, compareTransients, excludeFields);
        while (lhsClazz.getSuperclass() != null && lhsClazz != reflectUpToClass) {
            lhsClazz = lhsClazz.getSuperclass();
            reflectionAppend(lhs, rhs, lhsClazz, compareToBuilder, compareTransients, excludeFields);
        }
        return compareToBuilder.toComparison();
    }

    /**
     * Appends to {@code builder} the comparison of {@code lhs}
     * to {@code rhs} using the fields defined in {@code clazz}.
     *
     * @param lhs  left-hand object
     * @param rhs  right-hand object
     * @param clazz  {@link Class} that defines fields to be compared
     * @param builder  {@link CompareToBuilder} to append to
     * @param useTransients  whether to compare transient fields
     * @param excludeFields  fields to exclude
     */
    private static void reflectionAppend(
        final Object lhs,
        final Object rhs,
        final Class<?> clazz,
        final CompareToBuilder builder,
        final boolean useTransients,
        final String[] excludeFields) {

        final Field[] fields = clazz.getDeclaredFields();
        AccessibleObject.setAccessible(fields, true);
        for (int i = 0; i < fields.length && builder.comparison == 0; i++) {
            final Field f = fields[i];
            if (!ArrayUtils.contains(excludeFields, f.getName())
                && !f.getName().contains("$")
                && (useTransients || !Modifier.isTransient(f.getModifiers()))
                && !Modifier.isStatic(f.getModifiers())) {
                try {
                    builder.append(f.get(lhs), f.get(rhs));
                } catch (final IllegalAccessException e) {
                    // This can't happen. Would get a Security exception instead.
                    // Throw a runtime exception in case the impossible happens.
                    throw new InternalError("Unexpected IllegalAccessException");
                }
            }
        }
    }

    /**
     * Appends to the {@code builder} the {@code compareTo(Object)}
     * result of the superclass.
     *
     * @param superCompareTo  result of calling {@code super.compareTo(Object)}
     * @return this - used to chain append calls
     * @since 2.0
     */
    public CompareToBuilder appendSuper(final int superCompareTo) {
        if (comparison != 0) {
            return this;
        }
        comparison = superCompareTo;
        return this;
    }

    /**
     * Appends to the {@code builder} the comparison of
     * two {@link Object}s.
     *
     * <ol>
     * <li>Check if {@code lhs == rhs}</li>
     * <li>Check if either {@code lhs} or {@code rhs} is {@code null},
     *     a {@code null} object is less than a non-{@code null} object</li>
     * <li>Check the object contents</li>
     * </ol>
     *
     * <p>{@code lhs} must either be an array or implement {@link Comparable}.</p>
     *
     * @param lhs  left-hand object
     * @param rhs  right-hand object
     * @return this - used to chain append calls
     * @throws ClassCastException  if {@code rhs} is not assignment-compatible
     *  with {@code lhs}
     */
    public CompareToBuilder append(final Object lhs, final Object rhs) {
        return append(lhs, rhs, null);
    }

    /**
     * Appends to the {@code builder} the comparison of
     * two {@link Object}s.
     *
     * <ol>
     * <li>Check if {@code lhs == rhs}</li>
     * <li>Check if either {@code lhs} or {@code rhs} is {@code null},
     *     a {@code null} object is less than a non-{@code null} object</li>
     * <li>Check the object contents</li>
     * </ol>
     *
     * <p>If {@code lhs} is an array, array comparison methods will be used.
     * Otherwise {@code comparator} will be used to compare the objects.
     * If {@code comparator} is {@code null}, {@code lhs} must
     * implement {@link Comparable} instead.</p>
     *
     * @param lhs  left-hand object
     * @param rhs  right-hand object
     * @param comparator  {@link Comparator} used to compare the objects,
     *  {@code null} means treat lhs as {@link Comparable}
     * @return this - used to chain append calls
     * @throws ClassCastException  if {@code rhs} is not assignment-compatible
     *  with {@code lhs}
     * @since 2.0
     */
    public CompareToBuilder append(final Object lhs, final Object rhs, final Comparator<?> comparator) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = 1;
            return this;
        }
        if (ObjectUtils.isArray(lhs)) {
            // factor out array case in order to keep method small enough to be inlined
            appendArray(lhs, rhs, comparator);
        } else // the simple case, not an array, just test the element
        if (comparator == null) {
            @SuppressWarnings("unchecked") // assume this can be done; if not throw CCE as per Javadoc
            final Comparable<Object> comparable = (Comparable<Object>) lhs;
            comparison = comparable.compareTo(rhs);
        } else {
            @SuppressWarnings("unchecked") // assume this can be done; if not throw CCE as per Javadoc
            final Comparator<Object> comparator2 = (Comparator<Object>) comparator;
            comparison = comparator2.compare(lhs, rhs);
        }
        return this;
    }

    private void appendArray(final Object lhs, final Object rhs, final Comparator<?> comparator) {
        // switch on type of array, to dispatch to the correct handler
        // handles multidimensional arrays
        // throws a ClassCastException if rhs is not the correct array type
        if (lhs instanceof long[]) {
            append((long[]) lhs, (long[]) rhs);
        } else if (lhs instanceof int[]) {
            append((int[]) lhs, (int[]) rhs);
        } else if (lhs instanceof short[]) {
            append((short[]) lhs, (short[]) rhs);
        } else if (lhs instanceof char[]) {
            append((char[]) lhs, (char[]) rhs);
        } else if (lhs instanceof byte[]) {
            append((byte[]) lhs, (byte[]) rhs);
        } else if (lhs instanceof double[]) {
            append((double[]) lhs, (double[]) rhs);
        } else if (lhs instanceof float[]) {
            append((float[]) lhs, (float[]) rhs);
        } else if (lhs instanceof boolean[]) {
            append((boolean[]) lhs, (boolean[]) rhs);
        } else {
            // not an array of primitives
            // throws a ClassCastException if rhs is not an array
            append((Object[]) lhs, (Object[]) rhs, comparator);
        }
    }

    /**
     * Appends to the {@code builder} the comparison of
     * two {@code long}s.
     *
     * @param lhs  left-hand value
     * @param rhs  right-hand value
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final long lhs, final long rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = Long.compare(lhs, rhs);
        return this;
    }

    /**
     * Appends to the {@code builder} the comparison of
     * two {@code int}s.
     *
     * @param lhs  left-hand value
     * @param rhs  right-hand value
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final int lhs, final int rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = Integer.compare(lhs, rhs);
        return this;
    }

    /**
     * Appends to the {@code builder} the comparison of
     * two {@code short}s.
     *
     * @param lhs  left-hand value
     * @param rhs  right-hand value
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final short lhs, final short rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = Short.compare(lhs, rhs);
        return this;
    }

    /**
     * Appends to the {@code builder} the comparison of
     * two {@code char}s.
     *
     * @param lhs  left-hand value
     * @param rhs  right-hand value
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final char lhs, final char rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = Character.compare(lhs, rhs);
        return this;
    }

    /**
     * Appends to the {@code builder} the comparison of
     * two {@code byte}s.
     *
     * @param lhs  left-hand value
     * @param rhs  right-hand value
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final byte lhs, final byte rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = Byte.compare(lhs, rhs);
        return this;
    }

    /**
     * Appends to the {@code builder} the comparison of
     * two {@code double}s.
     *
     * <p>This handles NaNs, Infinities, and {@code -0.0}.</p>
     *
     * <p>It is compatible with the hash code generated by
     * {@link HashCodeBuilder}.</p>
     *
     * @param lhs  left-hand value
     * @param rhs  right-hand value
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final double lhs, final double rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = Double.compare(lhs, rhs);
        return this;
    }

    /**
     * Appends to the {@code builder} the comparison of
     * two {@code float}s.
     *
     * <p>This handles NaNs, Infinities, and {@code -0.0}.</p>
     *
     * <p>It is compatible with the hash code generated by
     * {@link HashCodeBuilder}.</p>
     *
     * @param lhs  left-hand value
     * @param rhs  right-hand value
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final float lhs, final float rhs) {
        if (comparison != 0) {
            return this;
        }
        comparison = Float.compare(lhs, rhs);
        return this;
    }

    /**
     * Appends to the {@code builder} the comparison of
     * two {@code booleans}s.
     *
     * @param lhs  left-hand value
     * @param rhs  right-hand value
     * @return this - used to chain append calls
      */
    public CompareToBuilder append(final boolean lhs, final boolean rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs) {
            comparison = 1;
        } else {
            comparison = -1;
        }
        return this;
    }

    /**
     * Appends to the {@code builder} the deep comparison of
     * two {@link Object} arrays.
     *
     * <ol>
     *  <li>Check if arrays are the same using {@code ==}</li>
     *  <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
     *  <li>Check array length, a short length array is less than a long length array</li>
     *  <li>Check array contents element by element using {@link #append(Object, Object, Comparator)}</li>
     * </ol>
     *
     * <p>This method will also will be called for the top level of multi-dimensional,
     * ragged, and multi-typed arrays.</p>
     *
     * @param lhs  left-hand array
     * @param rhs  right-hand array
     * @return this - used to chain append calls
     * @throws ClassCastException  if {@code rhs} is not assignment-compatible
     *  with {@code lhs}
     */
    public CompareToBuilder append(final Object[] lhs, final Object[] rhs) {
        return append(lhs, rhs, null);
    }

    /**
     * Appends to the {@code builder} the deep comparison of
     * two {@link Object} arrays.
     *
     * <ol>
     *  <li>Check if arrays are the same using {@code ==}</li>
     *  <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
     *  <li>Check array length, a short length array is less than a long length array</li>
     *  <li>Check array contents element by element using {@link #append(Object, Object, Comparator)}</li>
     * </ol>
     *
     * <p>This method will also will be called for the top level of multi-dimensional,
     * ragged, and multi-typed arrays.</p>
     *
     * @param lhs  left-hand array
     * @param rhs  right-hand array
     * @param comparator  {@link Comparator} to use to compare the array elements,
     *  {@code null} means to treat {@code lhs} elements as {@link Comparable}.
     * @return this - used to chain append calls
     * @throws ClassCastException  if {@code rhs} is not assignment-compatible
     *  with {@code lhs}
     * @since 2.0
     */
    public CompareToBuilder append(final Object[] lhs, final Object[] rhs, final Comparator<?> comparator) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = 1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = lhs.length < rhs.length ? -1 : 1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i], comparator);
        }
        return this;
    }

    /**
     * Appends to the {@code builder} the deep comparison of
     * two {@code long} arrays.
     *
     * <ol>
     *  <li>Check if arrays are the same using {@code ==}</li>
     *  <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(long, long)}</li>
     * </ol>
     *
     * @param lhs  left-hand array
     * @param rhs  right-hand array
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final long[] lhs, final long[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = 1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = lhs.length < rhs.length ? -1 : 1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * Appends to the {@code builder} the deep comparison of
     * two {@code int} arrays.
     *
     * <ol>
     *  <li>Check if arrays are the same using {@code ==}</li>
     *  <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(int, int)}</li>
     * </ol>
     *
     * @param lhs  left-hand array
     * @param rhs  right-hand array
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final int[] lhs, final int[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = 1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = lhs.length < rhs.length ? -1 : 1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * Appends to the {@code builder} the deep comparison of
     * two {@code short} arrays.
     *
     * <ol>
     *  <li>Check if arrays are the same using {@code ==}</li>
     *  <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(short, short)}</li>
     * </ol>
     *
     * @param lhs  left-hand array
     * @param rhs  right-hand array
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final short[] lhs, final short[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = 1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = lhs.length < rhs.length ? -1 : 1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * Appends to the {@code builder} the deep comparison of
     * two {@code char} arrays.
     *
     * <ol>
     *  <li>Check if arrays are the same using {@code ==}</li>
     *  <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(char, char)}</li>
     * </ol>
     *
     * @param lhs  left-hand array
     * @param rhs  right-hand array
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final char[] lhs, final char[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = 1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = lhs.length < rhs.length ? -1 : 1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * Appends to the {@code builder} the deep comparison of
     * two {@code byte} arrays.
     *
     * <ol>
     *  <li>Check if arrays are the same using {@code ==}</li>
     *  <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(byte, byte)}</li>
     * </ol>
     *
     * @param lhs  left-hand array
     * @param rhs  right-hand array
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final byte[] lhs, final byte[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = 1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = lhs.length < rhs.length ? -1 : 1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * Appends to the {@code builder} the deep comparison of
     * two {@code double} arrays.
     *
     * <ol>
     *  <li>Check if arrays are the same using {@code ==}</li>
     *  <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(double, double)}</li>
     * </ol>
     *
     * @param lhs  left-hand array
     * @param rhs  right-hand array
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final double[] lhs, final double[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = 1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = lhs.length < rhs.length ? -1 : 1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * Appends to the {@code builder} the deep comparison of
     * two {@code float} arrays.
     *
     * <ol>
     *  <li>Check if arrays are the same using {@code ==}</li>
     *  <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(float, float)}</li>
     * </ol>
     *
     * @param lhs  left-hand array
     * @param rhs  right-hand array
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final float[] lhs, final float[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = 1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = lhs.length < rhs.length ? -1 : 1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * Appends to the {@code builder} the deep comparison of
     * two {@code boolean} arrays.
     *
     * <ol>
     *  <li>Check if arrays are the same using {@code ==}</li>
     *  <li>Check if for {@code null}, {@code null} is less than non-{@code null}</li>
     *  <li>Check array length, a shorter length array is less than a longer length array</li>
     *  <li>Check array contents element by element using {@link #append(boolean, boolean)}</li>
     * </ol>
     *
     * @param lhs  left-hand array
     * @param rhs  right-hand array
     * @return this - used to chain append calls
     */
    public CompareToBuilder append(final boolean[] lhs, final boolean[] rhs) {
        if (comparison != 0) {
            return this;
        }
        if (lhs == rhs) {
            return this;
        }
        if (lhs == null) {
            comparison = -1;
            return this;
        }
        if (rhs == null) {
            comparison = 1;
            return this;
        }
        if (lhs.length != rhs.length) {
            comparison = lhs.length < rhs.length ? -1 : 1;
            return this;
        }
        for (int i = 0; i < lhs.length && comparison == 0; i++) {
            append(lhs[i], rhs[i]);
        }
        return this;
    }

    /**
     * Returns a negative integer, a positive integer, or zero as
     * the {@code builder} has judged the "left-hand" side
     * as less than, greater than, or equal to the "right-hand"
     * side.
     *
     * @return final comparison result
     * @see #build()
     */
    public int toComparison() {
        return comparison;
    }

    /**
     * Returns a negative Integer, a positive Integer, or zero as
     * the {@code builder} has judged the "left-hand" side
     * as less than, greater than, or equal to the "right-hand"
     * side.
     *
     * @return final comparison result as an Integer
     * @see #toComparison()
     * @since 3.0
     */
    @Override
    public Integer build() {
        return Integer.valueOf(toComparison());
    }
}

