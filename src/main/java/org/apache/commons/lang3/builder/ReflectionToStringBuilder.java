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
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Objects;

import org.apache.commons.lang3.ArraySorter;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.stream.Streams;

/**
 * Assists in implementing {@link Object#toString()} methods using reflection.
 *
 * <p>
 * This class uses reflection to determine the fields to append. Because these fields are usually private, the class
 * uses {@link java.lang.reflect.AccessibleObject#setAccessible(java.lang.reflect.AccessibleObject[], boolean)} to
 * change the visibility of the fields. This will fail under a security manager, unless the appropriate permissions are
 * set up correctly.
 * </p>
 * <p>
 * Using reflection to access (private) fields circumvents any synchronization protection guarding access to these
 * fields. If a toString method cannot safely read a field, you should exclude it from the toString method, or use
 * synchronization consistent with the class' lock management around the invocation of the method. Take special care to
 * exclude non-thread-safe collection classes, because these classes may throw ConcurrentModificationException if
 * modified while the toString method is executing.
 * </p>
 * <p>
 * A typical invocation for this method would look like:
 * </p>
 * <pre>
 * public String toString() {
 *     return ReflectionToStringBuilder.toString(this);
 * }
 * </pre>
 * <p>
 * You can also use the builder to debug 3rd party objects:
 * </p>
 * <pre>
 * System.out.println(&quot;An object: &quot; + ReflectionToStringBuilder.toString(anObject));
 * </pre>
 * <p>
 * A subclass can control field output by overriding the methods:
 * </p>
 * <ul>
 * <li>{@link #accept(java.lang.reflect.Field)}</li>
 * <li>{@link #getValue(java.lang.reflect.Field)}</li>
 * </ul>
 * <p>
 * For example, this method does <i>not</i> include the {@code password} field in the returned {@link String}:
 * </p>
 * <pre>
 * public String toString() {
 *     return (new ReflectionToStringBuilder(this) {
 *         protected boolean accept(Field f) {
 *             return super.accept(f) &amp;&amp; !f.getName().equals(&quot;password&quot;);
 *         }
 *     }).toString();
 * }
 * </pre>
 * <p>
 * Alternatively the {@link ToStringExclude} annotation can be used to exclude fields from being incorporated in the
 * result.
 * </p>
 * <p>
 * It is also possible to use the {@link ToStringSummary} annotation to output the summary information instead of the
 * detailed information of a field.
 * </p>
 * <p>
 * The exact format of the {@code toString} is determined by the {@link ToStringStyle} passed into the constructor.
 * </p>
 *
 * <p>
 * <b>Note:</b> the default {@link ToStringStyle} will only do a "shallow" formatting, i.e. composed objects are not
 * further traversed. To get "deep" formatting, use an instance of {@link RecursiveToStringStyle}.
 * </p>
 *
 * @since 2.0
 */
public class ReflectionToStringBuilder extends ToStringBuilder {

    /**
     * Converts the given Collection into an array of Strings. The returned array does not contain {@code null}
     * entries. Note that {@link Arrays#sort(Object[])} will throw an {@link NullPointerException} if an array element
     * is {@code null}.
     *
     * @param collection
     *            The collection to convert
     * @return A new array of Strings.
     */
    static String[] toNoNullStringArray(final Collection<String> collection) {
        if (collection == null) {
            return ArrayUtils.EMPTY_STRING_ARRAY;
        }
        return toNoNullStringArray(collection.toArray());
    }

    /**
     * Returns a new array of Strings without null elements. Internal method used to normalize exclude lists
     * (arrays and collections). Note that {@link Arrays#sort(Object[])} will throw an {@link NullPointerException}
     * if an array element is {@code null}.
     *
     * @param array
     *            The array to check
     * @return The given array or a new array without null.
     */
    static String[] toNoNullStringArray(final Object[] array) {
        return Streams.nonNull(array).map(Objects::toString).toArray(String[]::new);
    }

    /**
     * Builds a {@code toString} value using the default {@link ToStringStyle} through reflection.
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * Transient members will be not be included, as they are likely derived. Static fields will not be included.
     * Superclass fields will be appended.
     * </p>
     *
     * @param object
     *            the Object to be output
     * @return the String result
     * @throws IllegalArgumentException
     *             if the Object is {@code null}
     *
     * @see ToStringExclude
     * @see ToStringSummary
     */
    public static String toString(final Object object) {
        return toString(object, null, false, false, null);
    }

    /**
     * Builds a {@code toString} value through reflection.
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * Transient members will be not be included, as they are likely derived. Static fields will not be included.
     * Superclass fields will be appended.
     * </p>
     *
     * <p>
     * If the style is {@code null}, the default {@link ToStringStyle} is used.
     * </p>
     *
     * @param object
     *            the Object to be output
     * @param style
     *            the style of the {@code toString} to create, may be {@code null}
     * @return the String result
     * @throws IllegalArgumentException
     *             if the Object or {@link ToStringStyle} is {@code null}
     *
     * @see ToStringExclude
     * @see ToStringSummary
     */
    public static String toString(final Object object, final ToStringStyle style) {
        return toString(object, style, false, false, null);
    }

    /**
     * Builds a {@code toString} value through reflection.
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * If the {@code outputTransients} is {@code true}, transient members will be output, otherwise they
     * are ignored, as they are likely derived fields, and not part of the value of the Object.
     * </p>
     *
     * <p>
     * Static fields will not be included. Superclass fields will be appended.
     * </p>
     *
     * <p>
     * If the style is {@code null}, the default {@link ToStringStyle} is used.
     * </p>
     *
     * @param object
     *            the Object to be output
     * @param style
     *            the style of the {@code toString} to create, may be {@code null}
     * @param outputTransients
     *            whether to include transient fields
     * @return the String result
     * @throws IllegalArgumentException
     *             if the Object is {@code null}
     *
     * @see ToStringExclude
     * @see ToStringSummary
     */
    public static String toString(final Object object, final ToStringStyle style, final boolean outputTransients) {
        return toString(object, style, outputTransients, false, null);
    }

    /**
     * Builds a {@code toString} value through reflection.
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * If the {@code outputTransients} is {@code true}, transient fields will be output, otherwise they
     * are ignored, as they are likely derived fields, and not part of the value of the Object.
     * </p>
     *
     * <p>
     * If the {@code outputStatics} is {@code true}, static fields will be output, otherwise they are
     * ignored.
     * </p>
     *
     * <p>
     * Static fields will not be included. Superclass fields will be appended.
     * </p>
     *
     * <p>
     * If the style is {@code null}, the default {@link ToStringStyle} is used.
     * </p>
     *
     * @param object
     *            the Object to be output
     * @param style
     *            the style of the {@code toString} to create, may be {@code null}
     * @param outputTransients
     *            whether to include transient fields
     * @param outputStatics
     *            whether to include static fields
     * @return the String result
     * @throws IllegalArgumentException
     *             if the Object is {@code null}
     *
     * @see ToStringExclude
     * @see ToStringSummary
     * @since 2.1
     */
    public static String toString(final Object object, final ToStringStyle style, final boolean outputTransients, final boolean outputStatics) {
        return toString(object, style, outputTransients, outputStatics, null);
    }

    /**
     * Builds a {@code toString} value through reflection.
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * If the {@code outputTransients} is {@code true}, transient fields will be output, otherwise they
     * are ignored, as they are likely derived fields, and not part of the value of the Object.
     * </p>
     *
     * <p>
     * If the {@code outputStatics} is {@code true}, static fields will be output, otherwise they are
     * ignored.
     * </p>
     *
     * <p>
     * Superclass fields will be appended up to and including the specified superclass. A null superclass is treated as
     * {@code java.lang.Object}.
     * </p>
     *
     * <p>
     * If the style is {@code null}, the default {@link ToStringStyle} is used.
     * </p>
     *
     * @param <T>
     *            the type of the object
     * @param object
     *            the Object to be output
     * @param style
     *            the style of the {@code toString} to create, may be {@code null}
     * @param outputTransients
     *            whether to include transient fields
     * @param outputStatics
     *            whether to include static fields
     * @param excludeNullValues
     *            whether to exclude fields whose values are null
     * @param reflectUpToClass
     *            the superclass to reflect up to (inclusive), may be {@code null}
     * @return the String result
     * @throws IllegalArgumentException
     *             if the Object is {@code null}
     *
     * @see ToStringExclude
     * @see ToStringSummary
     * @since 3.6
     */
    public static <T> String toString(
            final T object, final ToStringStyle style, final boolean outputTransients,
            final boolean outputStatics, final boolean excludeNullValues, final Class<? super T> reflectUpToClass) {
        return new ReflectionToStringBuilder(object, style, null, reflectUpToClass, outputTransients, outputStatics, excludeNullValues)
                .toString();
    }

    /**
     * Builds a {@code toString} value through reflection.
     *
     * <p>
     * It uses {@code AccessibleObject.setAccessible} to gain access to private fields. This means that it will
     * throw a security exception if run under a security manager, if the permissions are not set up correctly. It is
     * also not as efficient as testing explicitly.
     * </p>
     *
     * <p>
     * If the {@code outputTransients} is {@code true}, transient fields will be output, otherwise they
     * are ignored, as they are likely derived fields, and not part of the value of the Object.
     * </p>
     *
     * <p>
     * If the {@code outputStatics} is {@code true}, static fields will be output, otherwise they are
     * ignored.
     * </p>
     *
     * <p>
     * Superclass fields will be appended up to and including the specified superclass. A null superclass is treated as
     * {@code java.lang.Object}.
     * </p>
     *
     * <p>
     * If the style is {@code null}, the default {@link ToStringStyle} is used.
     * </p>
     *
     * @param <T>
     *            the type of the object
     * @param object
     *            the Object to be output
     * @param style
     *            the style of the {@code toString} to create, may be {@code null}
     * @param outputTransients
     *            whether to include transient fields
     * @param outputStatics
     *            whether to include static fields
     * @param reflectUpToClass
     *            the superclass to reflect up to (inclusive), may be {@code null}
     * @return the String result
     * @throws IllegalArgumentException
     *             if the Object is {@code null}
     *
     * @see ToStringExclude
     * @see ToStringSummary
     * @since 2.1
     */
    public static <T> String toString(
            final T object, final ToStringStyle style, final boolean outputTransients,
            final boolean outputStatics, final Class<? super T> reflectUpToClass) {
        return new ReflectionToStringBuilder(object, style, null, reflectUpToClass, outputTransients, outputStatics)
                .toString();
    }

    /**
     * Builds a String for a toString method excluding the given field names.
     *
     * @param object
     *            The object to "toString".
     * @param excludeFieldNames
     *            The field names to exclude. Null excludes nothing.
     * @return The toString value.
     */
    public static String toStringExclude(final Object object, final Collection<String> excludeFieldNames) {
        return toStringExclude(object, toNoNullStringArray(excludeFieldNames));
    }

    /**
     * Builds a String for a toString method excluding the given field names.
     *
     * @param object
     *            The object to "toString".
     * @param excludeFieldNames
     *            The field names to exclude
     * @return The toString value.
     */
    public static String toStringExclude(final Object object, final String... excludeFieldNames) {
        return new ReflectionToStringBuilder(object).setExcludeFieldNames(excludeFieldNames).toString();
    }


    /**
     * Builds a String for a toString method including the given field names.
     *
     * @param object
     *            The object to "toString".
     * @param includeFieldNames
     *            {@code null} or empty means all fields are included. All fields are included by default. This method will override the default behavior.
     * @return The toString value.
     * @since 3.13.0
     */
    public static String toStringInclude(final Object object, final Collection<String> includeFieldNames) {
        return toStringInclude(object, toNoNullStringArray(includeFieldNames));
    }

    /**
     * Builds a String for a toString method including the given field names.
     *
     * @param object
     *            The object to "toString".
     * @param includeFieldNames
     *            The field names to include. {@code null} or empty means all fields are included. All fields are included by default. This method will override the default
     *             behavior.
     * @return The toString value.
     * @since 3.13.0
     */
    public static String toStringInclude(final Object object, final String... includeFieldNames) {
        return new ReflectionToStringBuilder(object).setIncludeFieldNames(includeFieldNames).toString();
    }

    /**
     * Whether or not to append static fields.
     */
    private boolean appendStatics;

    /**
     * Whether or not to append transient fields.
     */
    private boolean appendTransients;

    /**
     * Whether or not to append fields that are null.
     */
    private boolean excludeNullValues;

    /**
     * Which field names to exclude from output. Intended for fields like {@code "password"}.
     *
     * @since 3.0 this is protected instead of private
     */
    protected String[] excludeFieldNames;

    /**
     * Field names that will be included in the output. All fields are included by default.
     *
     * @since 3.13.0
     */
    protected String[] includeFieldNames;

    /**
     * The last super class to stop appending fields for.
     */
    private Class<?> upToClass;

    /**
     * Constructs a new instance.
     *
     * <p>
     * This constructor outputs using the default style set with {@code setDefaultStyle}.
     * </p>
     *
     * @param object
     *            the Object to build a {@code toString} for, must not be {@code null}
     * @throws IllegalArgumentException
     *             if the Object passed in is {@code null}
     */
    public ReflectionToStringBuilder(final Object object) {
        super(Objects.requireNonNull(object, "obj"));
    }

    /**
     * Constructs a new instance.
     *
     * <p>
     * If the style is {@code null}, the default style is used.
     * </p>
     *
     * @param object
     *            the Object to build a {@code toString} for, must not be {@code null}
     * @param style
     *            the style of the {@code toString} to create, may be {@code null}
     * @throws IllegalArgumentException
     *             if the Object passed in is {@code null}
     */
    public ReflectionToStringBuilder(final Object object, final ToStringStyle style) {
        super(Objects.requireNonNull(object, "obj"), style);
    }

    /**
     * Constructs a new instance.
     *
     * <p>
     * If the style is {@code null}, the default style is used.
     * </p>
     *
     * <p>
     * If the buffer is {@code null}, a new one is created.
     * </p>
     *
     * @param object
     *            the Object to build a {@code toString} for
     * @param style
     *            the style of the {@code toString} to create, may be {@code null}
     * @param buffer
     *            the {@link StringBuffer} to populate, may be {@code null}
     * @throws IllegalArgumentException
     *             if the Object passed in is {@code null}
     */
    public ReflectionToStringBuilder(final Object object, final ToStringStyle style, final StringBuffer buffer) {
        super(Objects.requireNonNull(object, "obj"), style, buffer);
    }

    /**
     * Constructs a new instance.
     *
     * @param <T>
     *            the type of the object
     * @param object
     *            the Object to build a {@code toString} for
     * @param style
     *            the style of the {@code toString} to create, may be {@code null}
     * @param buffer
     *            the {@link StringBuffer} to populate, may be {@code null}
     * @param reflectUpToClass
     *            the superclass to reflect up to (inclusive), may be {@code null}
     * @param outputTransients
     *            whether to include transient fields
     * @param outputStatics
     *            whether to include static fields
     * @since 2.1
     */
    public <T> ReflectionToStringBuilder(
            final T object, final ToStringStyle style, final StringBuffer buffer,
            final Class<? super T> reflectUpToClass, final boolean outputTransients, final boolean outputStatics) {
        super(Objects.requireNonNull(object, "obj"), style, buffer);
        this.setUpToClass(reflectUpToClass);
        this.setAppendTransients(outputTransients);
        this.setAppendStatics(outputStatics);
    }

    /**
     * Constructs a new instance.
     *
     * @param <T>
     *            the type of the object
     * @param object
     *            the Object to build a {@code toString} for
     * @param style
     *            the style of the {@code toString} to create, may be {@code null}
     * @param buffer
     *            the {@link StringBuffer} to populate, may be {@code null}
     * @param reflectUpToClass
     *            the superclass to reflect up to (inclusive), may be {@code null}
     * @param outputTransients
     *            whether to include transient fields
     * @param outputStatics
     *            whether to include static fields
     * @param excludeNullValues
     *            whether to exclude fields who value is null
     * @since 3.6
     */
    public <T> ReflectionToStringBuilder(
            final T object, final ToStringStyle style, final StringBuffer buffer,
            final Class<? super T> reflectUpToClass, final boolean outputTransients, final boolean outputStatics,
            final boolean excludeNullValues) {
        super(Objects.requireNonNull(object, "obj"), style, buffer);
        this.setUpToClass(reflectUpToClass);
        this.setAppendTransients(outputTransients);
        this.setAppendStatics(outputStatics);
        this.setExcludeNullValues(excludeNullValues);
    }

    /**
     * Returns whether or not to append the given {@link Field}.
     * <ul>
     * <li>Transient fields are appended only if {@link #isAppendTransients()} returns {@code true}.
     * <li>Static fields are appended only if {@link #isAppendStatics()} returns {@code true}.
     * <li>Inner class fields are not appended.</li>
     * </ul>
     *
     * @param field
     *            The Field to test.
     * @return Whether or not to append the given {@link Field}.
     */
    protected boolean accept(final Field field) {
        if (field.getName().indexOf(ClassUtils.INNER_CLASS_SEPARATOR_CHAR) != -1) {
            // Reject field from inner class.
            return false;
        }
        if (Modifier.isTransient(field.getModifiers()) && !this.isAppendTransients()) {
            // Reject transient fields.
            return false;
        }
        if (Modifier.isStatic(field.getModifiers()) && !this.isAppendStatics()) {
            // Reject static fields.
            return false;
        }

        if (this.excludeFieldNames != null
            && Arrays.binarySearch(this.excludeFieldNames, field.getName()) >= 0) {
            // Reject fields from the getExcludeFieldNames list.
            return false;
        }

        if (ArrayUtils.isNotEmpty(includeFieldNames)) {
            // Accept fields from the getIncludeFieldNames list. {@code null} or empty means all fields are included. All fields are included by default.
            return Arrays.binarySearch(this.includeFieldNames, field.getName()) >= 0;
        }

        return !field.isAnnotationPresent(ToStringExclude.class);
    }

    /**
     * Appends the fields and values defined by the given object of the given Class.
     *
     * <p>
     * If a cycle is detected as an object is &quot;toString()'ed&quot;, such an object is rendered as if
     * {@code Object.toString()} had been called and not implemented by the object.
     * </p>
     *
     * @param clazz
     *            The class of object parameter
     */
    protected void appendFieldsIn(final Class<?> clazz) {
        if (clazz.isArray()) {
            this.reflectionAppendArray(this.getObject());
            return;
        }
        // The elements in the returned array are not sorted and are not in any particular order.
        final Field[] fields = ArraySorter.sort(clazz.getDeclaredFields(), Comparator.comparing(Field::getName));
        AccessibleObject.setAccessible(fields, true);
        for (final Field field : fields) {
            final String fieldName = field.getName();
            if (this.accept(field)) {
                try {
                    // Warning: Field.get(Object) creates wrappers objects for primitive types.
                    final Object fieldValue = this.getValue(field);
                    if (!excludeNullValues || fieldValue != null) {
                        this.append(fieldName, fieldValue, !field.isAnnotationPresent(ToStringSummary.class));
                    }
                } catch (final IllegalAccessException ex) {
                    // this can't happen. Would get a Security exception instead
                    // throw a runtime exception in case the impossible happens.
                    throw new InternalError("Unexpected IllegalAccessException: " + ex.getMessage());
                }
            }
        }
    }

    /**
     * Gets the excludeFieldNames.
     *
     * @return the excludeFieldNames.
     */
    public String[] getExcludeFieldNames() {
        return this.excludeFieldNames.clone();
    }

    /**
     * Gets the includeFieldNames
     *
     * @return the includeFieldNames.
     * @since 3.13.0
     */
    public String[] getIncludeFieldNames() {
        return this.includeFieldNames.clone();
    }

    /**
     * Gets the last super class to stop appending fields for.
     *
     * @return The last super class to stop appending fields for.
     */
    public Class<?> getUpToClass() {
        return this.upToClass;
    }

    /**
     * Calls {@code java.lang.reflect.Field.get(Object)}.
     *
     * @param field
     *            The Field to query.
     * @return The Object from the given Field.
     *
     * @throws IllegalArgumentException
     *             see {@link java.lang.reflect.Field#get(Object)}
     * @throws IllegalAccessException
     *             see {@link java.lang.reflect.Field#get(Object)}
     *
     * @see java.lang.reflect.Field#get(Object)
     */
    protected Object getValue(final Field field) throws IllegalAccessException {
        return field.get(this.getObject());
    }

    /**
     * Gets whether or not to append static fields.
     *
     * @return Whether or not to append static fields.
     * @since 2.1
     */
    public boolean isAppendStatics() {
        return this.appendStatics;
    }

    /**
     * Gets whether or not to append transient fields.
     *
     * @return Whether or not to append transient fields.
     */
    public boolean isAppendTransients() {
        return this.appendTransients;
    }

    /**
     * Gets whether or not to append fields whose values are null.
     *
     * @return Whether or not to append fields whose values are null.
     * @since 3.6
     */
    public boolean isExcludeNullValues() {
        return this.excludeNullValues;
    }

    /**
     * Appends to the {@code toString} an {@link Object} array.
     *
     * @param array
     *            the array to add to the {@code toString}
     * @return this
     */
    public ReflectionToStringBuilder reflectionAppendArray(final Object array) {
        this.getStyle().reflectionAppendArrayDetail(this.getStringBuffer(), null, array);
        return this;
    }

    /**
     * Sets whether or not to append static fields.
     *
     * @param appendStatics
     *            Whether or not to append static fields.
     * @since 2.1
     */
    public void setAppendStatics(final boolean appendStatics) {
        this.appendStatics = appendStatics;
    }

    /**
     * Sets whether or not to append transient fields.
     *
     * @param appendTransients
     *            Whether or not to append transient fields.
     */
    public void setAppendTransients(final boolean appendTransients) {
        this.appendTransients = appendTransients;
    }

    /**
     * Sets the field names to exclude.
     *
     * @param excludeFieldNamesParam
     *            The excludeFieldNames to excluding from toString or {@code null}.
     * @return {@code this}
     */
    public ReflectionToStringBuilder setExcludeFieldNames(final String... excludeFieldNamesParam) {
        if (excludeFieldNamesParam == null) {
            this.excludeFieldNames = null;
        } else {
            // clone and remove nulls
            this.excludeFieldNames = ArraySorter.sort(toNoNullStringArray(excludeFieldNamesParam));
        }
        return this;
    }

    /**
     * Sets whether or not to append fields whose values are null.
     *
     * @param excludeNullValues
     *            Whether or not to append fields whose values are null.
     * @since 3.6
     */
    public void setExcludeNullValues(final boolean excludeNullValues) {
        this.excludeNullValues = excludeNullValues;
    }

    /**
     * Sets the field names to include. {@code null} or empty means all fields are included. All fields are included by default. This method will override the default behavior.
     *
     * @param includeFieldNamesParam
     *            The includeFieldNames that must be on toString or {@code null}.
     * @return {@code this}
     * @since 3.13.0
     */
    public ReflectionToStringBuilder setIncludeFieldNames(final String... includeFieldNamesParam) {
        if (includeFieldNamesParam == null) {
            this.includeFieldNames = null;
        } else {
            // clone and remove nulls
            this.includeFieldNames = ArraySorter.sort(toNoNullStringArray(includeFieldNamesParam));
        }
        return this;
    }

    /**
     * Sets the last super class to stop appending fields for.
     *
     * @param clazz
     *            The last super class to stop appending fields for.
     */
    public void setUpToClass(final Class<?> clazz) {
        if (clazz != null) {
            final Object object = getObject();
            if (object != null && !clazz.isInstance(object)) {
                throw new IllegalArgumentException("Specified class is not a superclass of the object");
            }
        }
        this.upToClass = clazz;
    }

    /**
     * Gets the String built by this builder.
     *
     * @return the built string
     */
    @Override
    public String toString() {
        if (this.getObject() == null) {
            return this.getStyle().getNullText();
        }

        validate();

        Class<?> clazz = this.getObject().getClass();
        this.appendFieldsIn(clazz);
        while (clazz.getSuperclass() != null && clazz != this.getUpToClass()) {
            clazz = clazz.getSuperclass();
            this.appendFieldsIn(clazz);
        }
        return super.toString();
    }

    /**
     * Validates that include and exclude names do not intersect.
     */
    private void validate() {
        if (ArrayUtils.containsAny(this.excludeFieldNames, (Object[]) this.includeFieldNames)) {
            ToStringStyle.unregister(this.getObject());
            throw new IllegalStateException("includeFieldNames and excludeFieldNames must not intersect");
        }
    }

}
