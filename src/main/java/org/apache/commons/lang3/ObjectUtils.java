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

import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.TreeSet;
import java.util.function.Supplier;
import java.util.stream.Stream;

import org.apache.commons.lang3.exception.CloneFailedException;
import org.apache.commons.lang3.function.Suppliers;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.commons.lang3.text.StrBuilder;
import org.apache.commons.lang3.time.DurationUtils;
import org.apache.commons.lang3.stream.Streams;

/**
 * Operations on {@link Object}.
 *
 * <p>This class tries to handle {@code null} input gracefully.
 * An exception will generally not be thrown for a {@code null} input.
 * Each method documents its behavior in more detail.</p>
 *
 * <p>#ThreadSafe#</p>
 * @since 1.0
 */
//@Immutable
@SuppressWarnings("deprecation") // deprecated class StrBuilder is imported
// because it is part of the signature of deprecated methods
public class ObjectUtils {

    /**
     * Class used as a null placeholder where {@code null}
     * has another meaning.
     *
     * <p>For example, in a {@link HashMap} the
     * {@link java.util.HashMap#get(Object)} method returns
     * {@code null} if the {@link Map} contains {@code null} or if there is
     * no matching key. The {@code null} placeholder can be used to distinguish
     * between these two cases.</p>
     *
     * <p>Another example is {@link Hashtable}, where {@code null}
     * cannot be stored.</p>
     */
    public static class Null implements Serializable {
        /**
         * Required for serialization support. Declare serialization compatibility with Commons Lang 1.0
         *
         * @see java.io.Serializable
         */
        private static final long serialVersionUID = 7092611880189329093L;

        /**
         * Restricted constructor - singleton.
         */
        Null() {
        }

        /**
         * Ensure Singleton after serialization.
         *
         * @return the singleton value
         */
        private Object readResolve() {
            return NULL;
        }
    }

    private static final char AT_SIGN = '@';

    /**
     * Singleton used as a {@code null} placeholder where
     * {@code null} has another meaning.
     *
     * <p>For example, in a {@link HashMap} the
     * {@link java.util.HashMap#get(Object)} method returns
     * {@code null} if the {@link Map} contains {@code null} or if there
     * is no matching key. The {@code null} placeholder can be used to
     * distinguish between these two cases.</p>
     *
     * <p>Another example is {@link Hashtable}, where {@code null}
     * cannot be stored.</p>
     *
     * <p>This instance is Serializable.</p>
     */
    public static final Null NULL = new Null();

    /**
     * Checks if all values in the array are not {@code nulls}.
     *
     * <p>
     * If any value is {@code null} or the array is {@code null} then
     * {@code false} is returned. If all elements in array are not
     * {@code null} or the array is empty (contains no elements) {@code true}
     * is returned.
     * </p>
     *
     * <pre>
     * ObjectUtils.allNotNull(*)             = true
     * ObjectUtils.allNotNull(*, *)          = true
     * ObjectUtils.allNotNull(null)          = false
     * ObjectUtils.allNotNull(null, null)    = false
     * ObjectUtils.allNotNull(null, *)       = false
     * ObjectUtils.allNotNull(*, null)       = false
     * ObjectUtils.allNotNull(*, *, null, *) = false
     * </pre>
     *
     * @param values  the values to test, may be {@code null} or empty
     * @return {@code false} if there is at least one {@code null} value in the array or the array is {@code null},
     * {@code true} if all values in the array are not {@code null}s or array contains no elements.
     * @since 3.5
     */
    public static boolean allNotNull(final Object... values) {
        return values != null && Stream.of(values).noneMatch(Objects::isNull);
    }

    /**
     * Checks if all values in the given array are {@code null}.
     *
     * <p>
     * If all the values are {@code null} or the array is {@code null}
     * or empty, then {@code true} is returned, otherwise {@code false} is returned.
     * </p>
     *
     * <pre>
     * ObjectUtils.allNull(*)                = false
     * ObjectUtils.allNull(*, null)          = false
     * ObjectUtils.allNull(null, *)          = false
     * ObjectUtils.allNull(null, null, *, *) = false
     * ObjectUtils.allNull(null)             = true
     * ObjectUtils.allNull(null, null)       = true
     * </pre>
     *
     * @param values  the values to test, may be {@code null} or empty
     * @return {@code true} if all values in the array are {@code null}s,
     * {@code false} if there is at least one non-null value in the array.
     * @since 3.11
     */
    public static boolean allNull(final Object... values) {
        return !anyNotNull(values);
    }

    /**
     * Checks if any value in the given array is not {@code null}.
     *
     * <p>
     * If all the values are {@code null} or the array is {@code null}
     * or empty then {@code false} is returned. Otherwise {@code true} is returned.
     * </p>
     *
     * <pre>
     * ObjectUtils.anyNotNull(*)                = true
     * ObjectUtils.anyNotNull(*, null)          = true
     * ObjectUtils.anyNotNull(null, *)          = true
     * ObjectUtils.anyNotNull(null, null, *, *) = true
     * ObjectUtils.anyNotNull(null)             = false
     * ObjectUtils.anyNotNull(null, null)       = false
     * </pre>
     *
     * @param values  the values to test, may be {@code null} or empty
     * @return {@code true} if there is at least one non-null value in the array,
     * {@code false} if all values in the array are {@code null}s.
     * If the array is {@code null} or empty {@code false} is also returned.
     * @since 3.5
     */
    public static boolean anyNotNull(final Object... values) {
        return firstNonNull(values) != null;
    }

    /**
     * Checks if any value in the given array is {@code null}.
     *
     * <p>
     * If any of the values are {@code null} or the array is {@code null},
     * then {@code true} is returned, otherwise {@code false} is returned.
     * </p>
     *
     * <pre>
     * ObjectUtils.anyNull(*)             = false
     * ObjectUtils.anyNull(*, *)          = false
     * ObjectUtils.anyNull(null)          = true
     * ObjectUtils.anyNull(null, null)    = true
     * ObjectUtils.anyNull(null, *)       = true
     * ObjectUtils.anyNull(*, null)       = true
     * ObjectUtils.anyNull(*, *, null, *) = true
     * </pre>
     *
     * @param values  the values to test, may be {@code null} or empty
     * @return {@code true} if there is at least one {@code null} value in the array,
     * {@code false} if all the values are non-null.
     * If the array is {@code null} or empty, {@code true} is also returned.
     * @since 3.11
     */
    public static boolean anyNull(final Object... values) {
        return !allNotNull(values);
    }

    /**
     * Clone an object.
     *
     * @param <T> the type of the object
     * @param obj  the object to clone, null returns null
     * @return the clone if the object implements {@link Cloneable} otherwise {@code null}
     * @throws CloneFailedException if the object is cloneable and the clone operation fails
     * @since 3.0
     */
    public static <T> T clone(final T obj) {
        if (obj instanceof Cloneable) {
            final Object result;
            if (isArray(obj)) {
                final Class<?> componentType = obj.getClass().getComponentType();
                if (componentType.isPrimitive()) {
                    int length = Array.getLength(obj);
                    result = Array.newInstance(componentType, length);
                    while (length-- > 0) {
                        Array.set(result, length, Array.get(obj, length));
                    }
                } else {
                    result = ((Object[]) obj).clone();
                }
            } else {
                try {
                    final Method clone = obj.getClass().getMethod("clone");
                    result = clone.invoke(obj);
                } catch (final NoSuchMethodException e) {
                    throw new CloneFailedException("Cloneable type "
                        + obj.getClass().getName()
                        + " has no clone method", e);
                } catch (final IllegalAccessException e) {
                    throw new CloneFailedException("Cannot clone Cloneable type "
                        + obj.getClass().getName(), e);
                } catch (final InvocationTargetException e) {
                    throw new CloneFailedException("Exception cloning Cloneable type "
                        + obj.getClass().getName(), e.getCause());
                }
            }
            @SuppressWarnings("unchecked") // OK because input is of type T
            final T checked = (T) result;
            return checked;
        }

        return null;
    }

    /**
     * Clone an object if possible.
     *
     * <p>This method is similar to {@link #clone(Object)}, but will return the provided
     * instance as the return value instead of {@code null} if the instance
     * is not cloneable. This is more convenient if the caller uses different
     * implementations (e.g. of a service) and some of the implementations do not allow concurrent
     * processing or have state. In such cases the implementation can simply provide a proper
     * clone implementation and the caller's code does not have to change.</p>
     *
     * @param <T> the type of the object
     * @param obj  the object to clone, null returns null
     * @return the clone if the object implements {@link Cloneable} otherwise the object itself
     * @throws CloneFailedException if the object is cloneable and the clone operation fails
     * @since 3.0
     */
    public static <T> T cloneIfPossible(final T obj) {
        final T clone = clone(obj);
        return clone == null ? obj : clone;
    }

    /**
     * Null safe comparison of Comparables.
     * {@code null} is assumed to be less than a non-{@code null} value.
     * <p>TODO Move to ComparableUtils.</p>
     *
     * @param <T> type of the values processed by this method
     * @param c1  the first comparable, may be null
     * @param c2  the second comparable, may be null
     * @return a negative value if c1 &lt; c2, zero if c1 = c2
     *  and a positive value if c1 &gt; c2
     */
    public static <T extends Comparable<? super T>> int compare(final T c1, final T c2) {
        return compare(c1, c2, false);
    }

    /**
     * Null safe comparison of Comparables.
     * <p>TODO Move to ComparableUtils.</p>
     *
     * @param <T> type of the values processed by this method
     * @param c1  the first comparable, may be null
     * @param c2  the second comparable, may be null
     * @param nullGreater if true {@code null} is considered greater
     *  than a non-{@code null} value or if false {@code null} is
     *  considered less than a Non-{@code null} value
     * @return a negative value if c1 &lt; c2, zero if c1 = c2
     *  and a positive value if c1 &gt; c2
     * @see java.util.Comparator#compare(Object, Object)
     */
    public static <T extends Comparable<? super T>> int compare(final T c1, final T c2, final boolean nullGreater) {
        if (c1 == c2) {
            return 0;
        }
        if (c1 == null) {
            return nullGreater ? 1 : -1;
        }
        if (c2 == null) {
            return nullGreater ? -1 : 1;
        }
        return c1.compareTo(c2);
    }

    /**
     * This method returns the provided value unchanged.
     * This can prevent javac from inlining a constant
     * field, e.g.,
     *
     * <pre>
     *     public final static boolean MAGIC_FLAG = ObjectUtils.CONST(true);
     * </pre>
     *
     * This way any jars that refer to this field do not
     * have to recompile themselves if the field's value
     * changes at some future date.
     *
     * @param v the boolean value to return
     * @return the boolean v, unchanged
     * @since 3.2
     */
    public static boolean CONST(final boolean v) {
        return v;
    }

    /**
     * This method returns the provided value unchanged.
     * This can prevent javac from inlining a constant
     * field, e.g.,
     *
     * <pre>
     *     public final static byte MAGIC_BYTE = ObjectUtils.CONST((byte) 127);
     * </pre>
     *
     * This way any jars that refer to this field do not
     * have to recompile themselves if the field's value
     * changes at some future date.
     *
     * @param v the byte value to return
     * @return the byte v, unchanged
     * @since 3.2
     */
    public static byte CONST(final byte v) {
        return v;
    }

    /**
     * This method returns the provided value unchanged.
     * This can prevent javac from inlining a constant
     * field, e.g.,
     *
     * <pre>
     *     public final static char MAGIC_CHAR = ObjectUtils.CONST('a');
     * </pre>
     *
     * This way any jars that refer to this field do not
     * have to recompile themselves if the field's value
     * changes at some future date.
     *
     * @param v the char value to return
     * @return the char v, unchanged
     * @since 3.2
     */
    public static char CONST(final char v) {
        return v;
    }

    /**
     * This method returns the provided value unchanged.
     * This can prevent javac from inlining a constant
     * field, e.g.,
     *
     * <pre>
     *     public final static double MAGIC_DOUBLE = ObjectUtils.CONST(1.0);
     * </pre>
     *
     * This way any jars that refer to this field do not
     * have to recompile themselves if the field's value
     * changes at some future date.
     *
     * @param v the double value to return
     * @return the double v, unchanged
     * @since 3.2
     */
    public static double CONST(final double v) {
        return v;
    }

    /**
     * This method returns the provided value unchanged.
     * This can prevent javac from inlining a constant
     * field, e.g.,
     *
     * <pre>
     *     public final static float MAGIC_FLOAT = ObjectUtils.CONST(1.0f);
     * </pre>
     *
     * This way any jars that refer to this field do not
     * have to recompile themselves if the field's value
     * changes at some future date.
     *
     * @param v the float value to return
     * @return the float v, unchanged
     * @since 3.2
     */
    public static float CONST(final float v) {
        return v;
    }

    /**
     * This method returns the provided value unchanged.
     * This can prevent javac from inlining a constant
     * field, e.g.,
     *
     * <pre>
     *     public final static int MAGIC_INT = ObjectUtils.CONST(123);
     * </pre>
     *
     * This way any jars that refer to this field do not
     * have to recompile themselves if the field's value
     * changes at some future date.
     *
     * @param v the int value to return
     * @return the int v, unchanged
     * @since 3.2
     */
    public static int CONST(final int v) {
        return v;
    }

    /**
     * This method returns the provided value unchanged.
     * This can prevent javac from inlining a constant
     * field, e.g.,
     *
     * <pre>
     *     public final static long MAGIC_LONG = ObjectUtils.CONST(123L);
     * </pre>
     *
     * This way any jars that refer to this field do not
     * have to recompile themselves if the field's value
     * changes at some future date.
     *
     * @param v the long value to return
     * @return the long v, unchanged
     * @since 3.2
     */
    public static long CONST(final long v) {
        return v;
    }

    /**
     * This method returns the provided value unchanged.
     * This can prevent javac from inlining a constant
     * field, e.g.,
     *
     * <pre>
     *     public final static short MAGIC_SHORT = ObjectUtils.CONST((short) 123);
     * </pre>
     *
     * This way any jars that refer to this field do not
     * have to recompile themselves if the field's value
     * changes at some future date.
     *
     * @param v the short value to return
     * @return the short v, unchanged
     * @since 3.2
     */
    public static short CONST(final short v) {
        return v;
    }

    /**
     * This method returns the provided value unchanged.
     * This can prevent javac from inlining a constant
     * field, e.g.,
     *
     * <pre>
     *     public final static String MAGIC_STRING = ObjectUtils.CONST("abc");
     * </pre>
     *
     * This way any jars that refer to this field do not
     * have to recompile themselves if the field's value
     * changes at some future date.
     *
     * @param <T> the Object type
     * @param v the genericized Object value to return (typically a String).
     * @return the genericized Object v, unchanged (typically a String).
     * @since 3.2
     */
    public static <T> T CONST(final T v) {
        return v;
    }

    /**
     * This method returns the provided value unchanged.
     * This can prevent javac from inlining a constant
     * field, e.g.,
     *
     * <pre>
     *     public final static byte MAGIC_BYTE = ObjectUtils.CONST_BYTE(127);
     * </pre>
     *
     * This way any jars that refer to this field do not
     * have to recompile themselves if the field's value
     * changes at some future date.
     *
     * @param v the byte literal (as an int) value to return
     * @throws IllegalArgumentException if the value passed to v
     *         is larger than a byte, that is, smaller than -128 or
     *         larger than 127.
     * @return the byte v, unchanged
     * @since 3.2
     */
    public static byte CONST_BYTE(final int v) {
        if (v < Byte.MIN_VALUE || v > Byte.MAX_VALUE) {
            throw new IllegalArgumentException("Supplied value must be a valid byte literal between -128 and 127: [" + v + "]");
        }
        return (byte) v;
    }

    /**
     * This method returns the provided value unchanged.
     * This can prevent javac from inlining a constant
     * field, e.g.,
     *
     * <pre>
     *     public final static short MAGIC_SHORT = ObjectUtils.CONST_SHORT(127);
     * </pre>
     *
     * This way any jars that refer to this field do not
     * have to recompile themselves if the field's value
     * changes at some future date.
     *
     * @param v the short literal (as an int) value to return
     * @throws IllegalArgumentException if the value passed to v
     *         is larger than a short, that is, smaller than -32768 or
     *         larger than 32767.
     * @return the byte v, unchanged
     * @since 3.2
     */
    public static short CONST_SHORT(final int v) {
        if (v < Short.MIN_VALUE || v > Short.MAX_VALUE) {
            throw new IllegalArgumentException("Supplied value must be a valid byte literal between -32768 and 32767: [" + v + "]");
        }
        return (short) v;
    }

    /**
     * Returns a default value if the object passed is {@code null}.
     *
     * <pre>
     * ObjectUtils.defaultIfNull(null, null)      = null
     * ObjectUtils.defaultIfNull(null, "")        = ""
     * ObjectUtils.defaultIfNull(null, "zz")      = "zz"
     * ObjectUtils.defaultIfNull("abc", *)        = "abc"
     * ObjectUtils.defaultIfNull(Boolean.TRUE, *) = Boolean.TRUE
     * </pre>
     *
     * @param <T> the type of the object
     * @param object  the {@link Object} to test, may be {@code null}
     * @param defaultValue  the default value to return, may be {@code null}
     * @return {@code object} if it is not {@code null}, defaultValue otherwise
     * TODO Rename to getIfNull in 4.0
     */
    public static <T> T defaultIfNull(final T object, final T defaultValue) {
        return object != null ? object : defaultValue;
    }

    // Null-safe equals/hashCode
    /**
     * Compares two objects for equality, where either one or both
     * objects may be {@code null}.
     *
     * <pre>
     * ObjectUtils.equals(null, null)                  = true
     * ObjectUtils.equals(null, "")                    = false
     * ObjectUtils.equals("", null)                    = false
     * ObjectUtils.equals("", "")                      = true
     * ObjectUtils.equals(Boolean.TRUE, null)          = false
     * ObjectUtils.equals(Boolean.TRUE, "true")        = false
     * ObjectUtils.equals(Boolean.TRUE, Boolean.TRUE)  = true
     * ObjectUtils.equals(Boolean.TRUE, Boolean.FALSE) = false
     * </pre>
     *
     * @param object1  the first object, may be {@code null}
     * @param object2  the second object, may be {@code null}
     * @return {@code true} if the values of both objects are the same
     * @deprecated this method has been replaced by {@code java.util.Objects.equals(Object, Object)} in Java 7 and will
     * be removed from future releases.
     */
    @Deprecated
    public static boolean equals(final Object object1, final Object object2) {
        return Objects.equals(object1, object2);
    }

    /**
     * Returns the first value in the array which is not {@code null}.
     * If all the values are {@code null} or the array is {@code null}
     * or empty then {@code null} is returned.
     *
     * <pre>
     * ObjectUtils.firstNonNull(null, null)      = null
     * ObjectUtils.firstNonNull(null, "")        = ""
     * ObjectUtils.firstNonNull(null, null, "")  = ""
     * ObjectUtils.firstNonNull(null, "zz")      = "zz"
     * ObjectUtils.firstNonNull("abc", *)        = "abc"
     * ObjectUtils.firstNonNull(null, "xyz", *)  = "xyz"
     * ObjectUtils.firstNonNull(Boolean.TRUE, *) = Boolean.TRUE
     * ObjectUtils.firstNonNull()                = null
     * </pre>
     *
     * @param <T> the component type of the array
     * @param values  the values to test, may be {@code null} or empty
     * @return the first value from {@code values} which is not {@code null},
     *  or {@code null} if there are no non-null values
     * @since 3.0
     */
    @SafeVarargs
    public static <T> T firstNonNull(final T... values) {
        return Streams.of(values).filter(Objects::nonNull).findFirst().orElse(null);
    }

    /**
     * Delegates to {@link Object#getClass()} using generics.
     *
     * @param <T> The argument type or null.
     * @param object The argument.
     * @return The argument Class or null.
     * @since 3.13.0
     */
    @SuppressWarnings("unchecked")
    public static <T> Class<T> getClass(final T object) {
        return object == null ? null : (Class<T>) object.getClass();
    }

    /**
     * Executes the given suppliers in order and returns the first return
     * value where a value other than {@code null} is returned.
     * Once a non-{@code null} value is obtained, all following suppliers are
     * not executed anymore.
     * If all the return values are {@code null} or no suppliers are provided
     * then {@code null} is returned.
     *
     * <pre>
     * ObjectUtils.firstNonNullLazy(null, () -&gt; null) = null
     * ObjectUtils.firstNonNullLazy(() -&gt; null, () -&gt; "") = ""
     * ObjectUtils.firstNonNullLazy(() -&gt; "", () -&gt; throw new IllegalStateException()) = ""
     * ObjectUtils.firstNonNullLazy(() -&gt; null, () -&gt; "zz) = "zz"
     * ObjectUtils.firstNonNullLazy() = null
     * </pre>
     *
     * @param <T> the type of the return values
     * @param suppliers  the suppliers returning the values to test.
     *                   {@code null} values are ignored.
     *                   Suppliers may return {@code null} or a value of type @{code T}
     * @return the first return value from {@code suppliers} which is not {@code null},
     *  or {@code null} if there are no non-null values
     * @since 3.10
     */
    @SafeVarargs
    public static <T> T getFirstNonNull(final Supplier<T>... suppliers) {
        return Streams.of(suppliers).map(s -> s != null ? s.get() : null).filter(Objects::nonNull).findFirst().orElse(null);
    }

    /**
     * Returns the given {@code object} is it is non-null, otherwise returns the Supplier's {@link Supplier#get()}
     * value.
     *
     * <p>
     * The caller responsible for thread-safety and exception handling of default value supplier.
     * </p>
     *
     * <pre>
     * ObjectUtils.getIfNull(null, () -&gt; null)     = null
     * ObjectUtils.getIfNull(null, null)              = null
     * ObjectUtils.getIfNull(null, () -&gt; "")       = ""
     * ObjectUtils.getIfNull(null, () -&gt; "zz")     = "zz"
     * ObjectUtils.getIfNull("abc", *)                = "abc"
     * ObjectUtils.getIfNull(Boolean.TRUE, *)         = Boolean.TRUE
     * </pre>
     *
     * @param <T> the type of the object
     * @param object the {@link Object} to test, may be {@code null}
     * @param defaultSupplier the default value to return, may be {@code null}
     * @return {@code object} if it is not {@code null}, {@code defaultValueSupplier.get()} otherwise
     * @since 3.10
     */
    public static <T> T getIfNull(final T object, final Supplier<T> defaultSupplier) {
        return object != null ? object : Suppliers.get(defaultSupplier);
    }

    /**
     * Gets the hash code of an object returning zero when the
     * object is {@code null}.
     *
     * <pre>
     * ObjectUtils.hashCode(null)   = 0
     * ObjectUtils.hashCode(obj)    = obj.hashCode()
     * </pre>
     *
     * @param obj  the object to obtain the hash code of, may be {@code null}
     * @return the hash code of the object, or zero if null
     * @since 2.1
     * @deprecated this method has been replaced by {@code java.util.Objects.hashCode(Object)} in Java 7 and will be
     * removed in future releases
     */
    @Deprecated
    public static int hashCode(final Object obj) {
        // hashCode(Object) for performance vs. hashCodeMulti(Object[]), as hash code is often critical
        return Objects.hashCode(obj);
    }

    /**
     * Returns the hex hash code for the given object per {@link Objects#hashCode(Object)}.
     * <p>
     * Short hand for {@code Integer.toHexString(Objects.hashCode(object))}.
     * </p>
     *
     * @param object object for which the hashCode is to be calculated
     * @return Hash code in hexadecimal format.
     * @since 3.13.0
     */
    public static String hashCodeHex(final Object object) {
        return Integer.toHexString(Objects.hashCode(object));
    }


    /**
     * Gets the hash code for multiple objects.
     *
     * <p>This allows a hash code to be rapidly calculated for a number of objects.
     * The hash code for a single object is the <em>not</em> same as {@link #hashCode(Object)}.
     * The hash code for multiple objects is the same as that calculated by an
     * {@link ArrayList} containing the specified objects.</p>
     *
     * <pre>
     * ObjectUtils.hashCodeMulti()                 = 1
     * ObjectUtils.hashCodeMulti((Object[]) null)  = 1
     * ObjectUtils.hashCodeMulti(a)                = 31 + a.hashCode()
     * ObjectUtils.hashCodeMulti(a,b)              = (31 + a.hashCode()) * 31 + b.hashCode()
     * ObjectUtils.hashCodeMulti(a,b,c)            = ((31 + a.hashCode()) * 31 + b.hashCode()) * 31 + c.hashCode()
     * </pre>
     *
     * @param objects  the objects to obtain the hash code of, may be {@code null}
     * @return the hash code of the objects, or zero if null
     * @since 3.0
     * @deprecated this method has been replaced by {@code java.util.Objects.hash(Object...)} in Java 7 and will be
     * removed in future releases.
     */
    @Deprecated
    public static int hashCodeMulti(final Object... objects) {
        int hash = 1;
        if (objects != null) {
            for (final Object object : objects) {
                final int tmpHash = Objects.hashCode(object);
                hash = hash * 31 + tmpHash;
            }
        }
        return hash;
    }

    /**
     * Returns the hex hash code for the given object per {@link System#identityHashCode(Object)}.
     * <p>
     * Short hand for {@code Integer.toHexString(System.identityHashCode(object))}.
     * </p>
     *
     * @param object object for which the hashCode is to be calculated
     * @return Hash code in hexadecimal format.
     * @since 3.13.0
     */
    public static String identityHashCodeHex(final Object object) {
        return Integer.toHexString(System.identityHashCode(object));
    }

    /**
     * Appends the toString that would be produced by {@link Object}
     * if a class did not override toString itself. {@code null}
     * will throw a NullPointerException for either of the two parameters.
     *
     * <pre>
     * ObjectUtils.identityToString(appendable, "")            = appendable.append("java.lang.String@1e23")
     * ObjectUtils.identityToString(appendable, Boolean.TRUE)  = appendable.append("java.lang.Boolean@7fa")
     * ObjectUtils.identityToString(appendable, Boolean.TRUE)  = appendable.append("java.lang.Boolean@7fa")
     * </pre>
     *
     * @param appendable  the appendable to append to
     * @param object  the object to create a toString for
     * @throws IOException if an I/O error occurs.
     * @since 3.2
     */
    public static void identityToString(final Appendable appendable, final Object object) throws IOException {
        Validate.notNull(object, "object");
        appendable.append(object.getClass().getName())
              .append(AT_SIGN)
              .append(identityHashCodeHex(object));
    }

    /**
     * Gets the toString that would be produced by {@link Object}
     * if a class did not override toString itself. {@code null}
     * will return {@code null}.
     *
     * <pre>
     * ObjectUtils.identityToString(null)         = null
     * ObjectUtils.identityToString("")           = "java.lang.String@1e23"
     * ObjectUtils.identityToString(Boolean.TRUE) = "java.lang.Boolean@7fa"
     * </pre>
     *
     * @param object  the object to create a toString for, may be
     *  {@code null}
     * @return the default toString text, or {@code null} if
     *  {@code null} passed in
     */
    public static String identityToString(final Object object) {
        if (object == null) {
            return null;
        }
        final String name = object.getClass().getName();
        final String hexString = identityHashCodeHex(object);
        final StringBuilder builder = new StringBuilder(name.length() + 1 + hexString.length());
        // @formatter:off
        builder.append(name)
              .append(AT_SIGN)
              .append(hexString);
        // @formatter:on
        return builder.toString();
    }

    /**
     * Appends the toString that would be produced by {@link Object}
     * if a class did not override toString itself. {@code null}
     * will throw a NullPointerException for either of the two parameters.
     *
     * <pre>
     * ObjectUtils.identityToString(builder, "")            = builder.append("java.lang.String@1e23")
     * ObjectUtils.identityToString(builder, Boolean.TRUE)  = builder.append("java.lang.Boolean@7fa")
     * ObjectUtils.identityToString(builder, Boolean.TRUE)  = builder.append("java.lang.Boolean@7fa")
     * </pre>
     *
     * @param builder  the builder to append to
     * @param object  the object to create a toString for
     * @since 3.2
     * @deprecated as of 3.6, because StrBuilder was moved to commons-text,
     *  use one of the other {@code identityToString} methods instead
     */
    @Deprecated
    public static void identityToString(final StrBuilder builder, final Object object) {
        Validate.notNull(object, "object");
        final String name = object.getClass().getName();
        final String hexString = identityHashCodeHex(object);
        builder.ensureCapacity(builder.length() +  name.length() + 1 + hexString.length());
        builder.append(name)
              .append(AT_SIGN)
              .append(hexString);
    }

    /**
     * Appends the toString that would be produced by {@link Object}
     * if a class did not override toString itself. {@code null}
     * will throw a NullPointerException for either of the two parameters.
     *
     * <pre>
     * ObjectUtils.identityToString(buf, "")            = buf.append("java.lang.String@1e23")
     * ObjectUtils.identityToString(buf, Boolean.TRUE)  = buf.append("java.lang.Boolean@7fa")
     * ObjectUtils.identityToString(buf, Boolean.TRUE)  = buf.append("java.lang.Boolean@7fa")
     * </pre>
     *
     * @param buffer  the buffer to append to
     * @param object  the object to create a toString for
     * @since 2.4
     */
    public static void identityToString(final StringBuffer buffer, final Object object) {
        Validate.notNull(object, "object");
        final String name = object.getClass().getName();
        final String hexString = identityHashCodeHex(object);
        buffer.ensureCapacity(buffer.length() + name.length() + 1 + hexString.length());
        buffer.append(name)
              .append(AT_SIGN)
              .append(hexString);
    }

    /**
     * Appends the toString that would be produced by {@link Object}
     * if a class did not override toString itself. {@code null}
     * will throw a NullPointerException for either of the two parameters.
     *
     * <pre>
     * ObjectUtils.identityToString(builder, "")            = builder.append("java.lang.String@1e23")
     * ObjectUtils.identityToString(builder, Boolean.TRUE)  = builder.append("java.lang.Boolean@7fa")
     * ObjectUtils.identityToString(builder, Boolean.TRUE)  = builder.append("java.lang.Boolean@7fa")
     * </pre>
     *
     * @param builder  the builder to append to
     * @param object  the object to create a toString for
     * @since 3.2
     */
    public static void identityToString(final StringBuilder builder, final Object object) {
        Validate.notNull(object, "object");
        final String name = object.getClass().getName();
        final String hexString = identityHashCodeHex(object);
        builder.ensureCapacity(builder.length() +  name.length() + 1 + hexString.length());
        builder.append(name)
              .append(AT_SIGN)
              .append(hexString);
    }


    // Constants (LANG-816):
    /*
        These methods ensure constants are not inlined by javac.
        For example, typically a developer might declare a constant like so:

            public final static int MAGIC_NUMBER = 5;

        Should a different jar file refer to this, and the MAGIC_NUMBER
        is changed a later date (e.g., MAGIC_NUMBER = 6), the different jar
        file will need to recompile itself.  This is because javac
        typically inlines the primitive or String constant directly into
        the bytecode, and removes the reference to the MAGIC_NUMBER field.

        To help the other jar (so that it does not need to recompile
        when constants are changed) the original developer can declare
        their constant using one of the CONST() utility methods, instead:

            public final static int MAGIC_NUMBER = CONST(5);
     */

    /**
     * Checks, whether the given object is an Object array or a primitive array in a null-safe manner.
     *
     * <p>
     * A {@code null} {@code object} Object will return {@code false}.
     * </p>
     *
     * <pre>
     * ObjectUtils.isArray(null)             = false
     * ObjectUtils.isArray("")               = false
     * ObjectUtils.isArray("ab")             = false
     * ObjectUtils.isArray(new int[]{})      = true
     * ObjectUtils.isArray(new int[]{1,2,3}) = true
     * ObjectUtils.isArray(1234)             = false
     * </pre>
     *
     * @param object the object to check, may be {@code null}
     * @return {@code true} if the object is an {@code array}, {@code false} otherwise
     * @since 3.13.0
     */
    public static boolean isArray(final Object object) {
        return object != null && object.getClass().isArray();
    }

    /**
     * Checks if an Object is empty or null.
     *
     * The following types are supported:
     * <ul>
     * <li>{@link CharSequence}: Considered empty if its length is zero.</li>
     * <li>{@link Array}: Considered empty if its length is zero.</li>
     * <li>{@link Collection}: Considered empty if it has zero elements.</li>
     * <li>{@link Map}: Considered empty if it has zero key-value mappings.</li>
     * <li>{@link Optional}: Considered empty if {@link Optional#isPresent} returns false, regardless of the "emptiness" of the contents.</li>
     * </ul>
     *
     * <pre>
     * ObjectUtils.isEmpty(null)             = true
     * ObjectUtils.isEmpty("")               = true
     * ObjectUtils.isEmpty("ab")             = false
     * ObjectUtils.isEmpty(new int[]{})      = true
     * ObjectUtils.isEmpty(new int[]{1,2,3}) = false
     * ObjectUtils.isEmpty(1234)             = false
     * ObjectUtils.isEmpty(1234)             = false
     * ObjectUtils.isEmpty(Optional.of(""))  = false
     * ObjectUtils.isEmpty(Optional.empty()) = true
     * </pre>
     *
     * @param object  the {@link Object} to test, may be {@code null}
     * @return {@code true} if the object has a supported type and is empty or null,
     * {@code false} otherwise
     * @since 3.9
     */
    public static boolean isEmpty(final Object object) {
        if (object == null) {
            return true;
        }
        if (object instanceof CharSequence) {
            return ((CharSequence) object).length() == 0;
        }
        if (isArray(object)) {
            return Array.getLength(object) == 0;
        }
        if (object instanceof Collection<?>) {
            return ((Collection<?>) object).isEmpty();
        }
        if (object instanceof Map<?, ?>) {
            return ((Map<?, ?>) object).isEmpty();
        }
        if (object instanceof Optional<?>) {
            // TODO Java 11 Use Optional#isEmpty()
            return !((Optional<?>) object).isPresent();
        }
        return false;
    }

    /**
     * Checks if an Object is not empty and not null.
     *
     * The following types are supported:
     * <ul>
     * <li>{@link CharSequence}: Considered empty if its length is zero.</li>
     * <li>{@link Array}: Considered empty if its length is zero.</li>
     * <li>{@link Collection}: Considered empty if it has zero elements.</li>
     * <li>{@link Map}: Considered empty if it has zero key-value mappings.</li>
     * <li>{@link Optional}: Considered empty if {@link Optional#isPresent} returns false, regardless of the "emptiness" of the contents.</li>
     * </ul>
     *
     * <pre>
     * ObjectUtils.isNotEmpty(null)             = false
     * ObjectUtils.isNotEmpty("")               = false
     * ObjectUtils.isNotEmpty("ab")             = true
     * ObjectUtils.isNotEmpty(new int[]{})      = false
     * ObjectUtils.isNotEmpty(new int[]{1,2,3}) = true
     * ObjectUtils.isNotEmpty(1234)             = true
     * ObjectUtils.isNotEmpty(Optional.of(""))  = true
     * ObjectUtils.isNotEmpty(Optional.empty()) = false
     * </pre>
     *
     * @param object  the {@link Object} to test, may be {@code null}
     * @return {@code true} if the object has an unsupported type or is not empty
     * and not null, {@code false} otherwise
     * @since 3.9
     */
    public static boolean isNotEmpty(final Object object) {
        return !isEmpty(object);
    }

    /**
     * Null safe comparison of Comparables.
     * <p>TODO Move to ComparableUtils.</p>
     *
     * @param <T> type of the values processed by this method
     * @param values the set of comparable values, may be null
     * @return
     *  <ul>
     *   <li>If any objects are non-null and unequal, the greater object.
     *   <li>If all objects are non-null and equal, the first.
     *   <li>If any of the comparables are null, the greater of the non-null objects.
     *   <li>If all the comparables are null, null is returned.
     *  </ul>
     */
    @SafeVarargs
    public static <T extends Comparable<? super T>> T max(final T... values) {
        T result = null;
        if (values != null) {
            for (final T value : values) {
                if (compare(value, result, false) > 0) {
                    result = value;
                }
            }
        }
        return result;
    }

    /**
     * Find the "best guess" middle value among comparables. If there is an even
     * number of total values, the lower of the two middle values will be returned.
     * @param <T> type of values processed by this method
     * @param comparator to use for comparisons
     * @param items to compare
     * @return T at middle position
     * @throws NullPointerException if items or comparator is {@code null}
     * @throws IllegalArgumentException if items is empty or contains {@code null} values
     * @since 3.0.1
     */
    @SafeVarargs
    public static <T> T median(final Comparator<T> comparator, final T... items) {
        Validate.notEmpty(items, "null/empty items");
        Validate.noNullElements(items);
        Validate.notNull(comparator, "comparator");
        final TreeSet<T> treeSet = new TreeSet<>(comparator);
        Collections.addAll(treeSet, items);
        @SuppressWarnings("unchecked") //we know all items added were T instances
        final T result = (T) treeSet.toArray()[(treeSet.size() - 1) / 2];
        return result;
    }

    /**
     * Find the "best guess" middle value among comparables. If there is an even
     * number of total values, the lower of the two middle values will be returned.
     * @param <T> type of values processed by this method
     * @param items to compare
     * @return T at middle position
     * @throws NullPointerException if items is {@code null}
     * @throws IllegalArgumentException if items is empty or contains {@code null} values
     * @since 3.0.1
     */
    @SafeVarargs
    public static <T extends Comparable<? super T>> T median(final T... items) {
        Validate.notEmpty(items);
        Validate.noNullElements(items);
        final TreeSet<T> sort = new TreeSet<>();
        Collections.addAll(sort, items);
        @SuppressWarnings("unchecked") //we know all items added were T instances
        final T result = (T) sort.toArray()[(sort.size() - 1) / 2];
        return result;
    }

    /**
     * Null safe comparison of Comparables.
     * <p>TODO Move to ComparableUtils.</p>
     *
     * @param <T> type of the values processed by this method
     * @param values the set of comparable values, may be null
     * @return
     *  <ul>
     *   <li>If any objects are non-null and unequal, the lesser object.
     *   <li>If all objects are non-null and equal, the first.
     *   <li>If any of the comparables are null, the lesser of the non-null objects.
     *   <li>If all the comparables are null, null is returned.
     *  </ul>
     */
    @SafeVarargs
    public static <T extends Comparable<? super T>> T min(final T... values) {
        T result = null;
        if (values != null) {
            for (final T value : values) {
                if (compare(value, result, true) < 0) {
                    result = value;
                }
            }
        }
        return result;
    }


    /**
     * Find the most frequently occurring item.
     *
     * @param <T> type of values processed by this method
     * @param items to check
     * @return most populous T, {@code null} if non-unique or no items supplied
     * @since 3.0.1
     */
    @SafeVarargs
    public static <T> T mode(final T... items) {
        if (ArrayUtils.isNotEmpty(items)) {
            final HashMap<T, MutableInt> occurrences = new HashMap<>(items.length);
            for (final T t : items) {
                final MutableInt count = occurrences.get(t);
                if (count == null) {
                    occurrences.put(t, new MutableInt(1));
                } else {
                    count.increment();
                }
            }
            T result = null;
            int max = 0;
            for (final Map.Entry<T, MutableInt> e : occurrences.entrySet()) {
                final int cmp = e.getValue().intValue();
                if (cmp == max) {
                    result = null;
                } else if (cmp > max) {
                    max = cmp;
                    result = e.getKey();
                }
            }
            return result;
        }
        return null;
    }

    /**
     * Compares two objects for inequality, where either one or both
     * objects may be {@code null}.
     *
     * <pre>
     * ObjectUtils.notEqual(null, null)                  = false
     * ObjectUtils.notEqual(null, "")                    = true
     * ObjectUtils.notEqual("", null)                    = true
     * ObjectUtils.notEqual("", "")                      = false
     * ObjectUtils.notEqual(Boolean.TRUE, null)          = true
     * ObjectUtils.notEqual(Boolean.TRUE, "true")        = true
     * ObjectUtils.notEqual(Boolean.TRUE, Boolean.TRUE)  = false
     * ObjectUtils.notEqual(Boolean.TRUE, Boolean.FALSE) = true
     * </pre>
     *
     * @param object1  the first object, may be {@code null}
     * @param object2  the second object, may be {@code null}
     * @return {@code false} if the values of both objects are the same
     */
    public static boolean notEqual(final Object object1, final Object object2) {
        return !Objects.equals(object1, object2);
    }

    /**
     * Checks that the specified object reference is not {@code null} or empty per {@link #isEmpty(Object)}. Use this
     * method for validation, for example:
     *
     * <blockquote>
     *
     * <pre>
     * public Foo(Bar bar) {
     *     this.bar = Objects.requireNonEmpty(bar);
     * }
     * </pre>
     *
     * </blockquote>
     *
     * @param <T> the type of the reference.
     * @param obj the object reference to check for nullity.
     * @return {@code obj} if not {@code null}.
     * @throws NullPointerException     if {@code obj} is {@code null}.
     * @throws IllegalArgumentException if {@code obj} is empty per {@link #isEmpty(Object)}.
     * @see #isEmpty(Object)
     * @since 3.12.0
     */
    public static <T> T  requireNonEmpty(final T obj) {
        return requireNonEmpty(obj, "object");
    }

    /**
     * Checks that the specified object reference is not {@code null} or empty per {@link #isEmpty(Object)}. Use this
     * method for validation, for example:
     *
     * <blockquote>
     *
     * <pre>
     * public Foo(Bar bar) {
     *     this.bar = Objects.requireNonEmpty(bar, "bar");
     * }
     * </pre>
     *
     * </blockquote>
     *
     * @param <T> the type of the reference.
     * @param obj the object reference to check for nullity.
     * @param message the exception message.
     * @return {@code obj} if not {@code null}.
     * @throws NullPointerException     if {@code obj} is {@code null}.
     * @throws IllegalArgumentException if {@code obj} is empty per {@link #isEmpty(Object)}.
     * @see #isEmpty(Object)
     * @since 3.12.0
     */
    public static <T> T requireNonEmpty(final T obj, final String message) {
        // check for null first to give the most precise exception.
        Objects.requireNonNull(obj, message);
        if (isEmpty(obj)) {
            throw new IllegalArgumentException(message);
        }
        return obj;
    }

    /**
     * Gets the {@code toString} of an {@link Object} returning
     * an empty string ("") if {@code null} input.
     *
     * <pre>
     * ObjectUtils.toString(null)         = ""
     * ObjectUtils.toString("")           = ""
     * ObjectUtils.toString("bat")        = "bat"
     * ObjectUtils.toString(Boolean.TRUE) = "true"
     * </pre>
     *
     * @see StringUtils#defaultString(String)
     * @see String#valueOf(Object)
     * @param obj  the Object to {@code toString}, may be null
     * @return the passed in Object's toString, or {@code ""} if {@code null} input
     * @since 2.0
     * @deprecated this method has been replaced by {@code java.util.Objects.toString(Object)} in Java 7 and will be
     * removed in future releases. Note however that said method will return "null" for null references, while this
     * method returns an empty String. To preserve behavior use {@code java.util.Objects.toString(myObject, "")}
     */
    @Deprecated
    public static String toString(final Object obj) {
        return obj == null ? StringUtils.EMPTY : obj.toString();
    }

    /**
     * Gets the {@code toString} of an {@link Object} returning
     * a specified text if {@code null} input.
     *
     * <pre>
     * ObjectUtils.toString(null, null)           = null
     * ObjectUtils.toString(null, "null")         = "null"
     * ObjectUtils.toString("", "null")           = ""
     * ObjectUtils.toString("bat", "null")        = "bat"
     * ObjectUtils.toString(Boolean.TRUE, "null") = "true"
     * </pre>
     *
     * @see StringUtils#defaultString(String,String)
     * @see String#valueOf(Object)
     * @param obj  the Object to {@code toString}, may be null
     * @param nullStr  the String to return if {@code null} input, may be null
     * @return the passed in Object's toString, or {@code nullStr} if {@code null} input
     * @since 2.0
     * @deprecated this method has been replaced by {@code java.util.Objects.toString(Object, String)} in Java 7 and
     * will be removed in future releases.
     */
    @Deprecated
    public static String toString(final Object obj, final String nullStr) {
        return obj == null ? nullStr : obj.toString();
    }

    /**
     * Gets the {@code toString} of an {@link Object} returning
     * a specified text if {@code null} input.
     *
     * <pre>
     * ObjectUtils.toString(obj, () -&gt; expensive())
     * </pre>
     * <pre>
     * ObjectUtils.toString(null, () -&gt; expensive())         = result of expensive()
     * ObjectUtils.toString(null, () -&gt; expensive())         = result of expensive()
     * ObjectUtils.toString("", () -&gt; expensive())           = ""
     * ObjectUtils.toString("bat", () -&gt; expensive())        = "bat"
     * ObjectUtils.toString(Boolean.TRUE, () -&gt; expensive()) = "true"
     * </pre>
     *
     * @param obj  the Object to {@code toString}, may be null
     * @param supplier  the Supplier of String used on {@code null} input, may be null
     * @return the passed in Object's toString, or {@code nullStr} if {@code null} input
     * @since 3.11
     */
    public static String toString(final Object obj, final Supplier<String> supplier) {
        return obj == null ? Suppliers.get(supplier) : obj.toString();
    }

    /**
     * Calls {@link Object#wait(long, int)} for the given Duration.
     *
     * @param obj The receiver of the wait call.
     * @param duration How long to wait.
     * @throws IllegalArgumentException if the timeout duration is negative.
     * @throws IllegalMonitorStateException if the current thread is not the owner of the {@code obj}'s monitor.
     * @throws InterruptedException if any thread interrupted the current thread before or while the current thread was
     *         waiting for a notification. The <em>interrupted status</em> of the current thread is cleared when this
     *         exception is thrown.
     * @see Object#wait(long, int)
     * @since 3.12.0
     */
    public static void wait(final Object obj, final Duration duration) throws InterruptedException {
        DurationUtils.accept(obj::wait, DurationUtils.zeroIfNull(duration));
    }

    /**
     * {@link ObjectUtils} instances should NOT be constructed in
     * standard programming. Instead, the static methods on the class should
     * be used, such as {@code ObjectUtils.defaultIfNull("a","b");}.
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public ObjectUtils() {
    }

}
