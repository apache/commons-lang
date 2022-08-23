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

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Arrays;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.lang3.exception.UncheckedException;

/**
 * Helper methods for working with {@link Annotation} instances.
 *
 * <p>This class contains various utility methods that make working with
 * annotations simpler.</p>
 *
 * <p>{@link Annotation} instances are always proxy objects; unfortunately
 * dynamic proxies cannot be depended upon to know how to implement certain
 * methods in the same manner as would be done by "natural" {@link Annotation}s.
 * The methods presented in this class can be used to avoid that possibility. It
 * is of course also possible for dynamic proxies to actually delegate their
 * e.g. {@link Annotation#equals(Object)}/{@link Annotation#hashCode()}/
 * {@link Annotation#toString()} implementations to {@link AnnotationUtils}.</p>
 *
 * <p>#ThreadSafe#</p>
 *
 * @since 3.0
 */
public class AnnotationUtils {

    /**
     * A style that prints annotations as recommended.
     */
    private static final ToStringStyle TO_STRING_STYLE = new ToStringStyle() {
        /** Serialization version */
        private static final long serialVersionUID = 1L;

        {
            setDefaultFullDetail(true);
            setArrayContentDetail(true);
            setUseClassName(true);
            setUseShortClassName(true);
            setUseIdentityHashCode(false);
            setContentStart("(");
            setContentEnd(")");
            setFieldSeparator(", ");
            setArrayStart("[");
            setArrayEnd("]");
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected String getShortClassName(final Class<?> cls) {
            // formatter:off
            return ClassUtils.getAllInterfaces(cls).stream().filter(Annotation.class::isAssignableFrom).findFirst()
                .map(iface -> "@" + iface.getName())
                .orElse(StringUtils.EMPTY);
            // formatter:on
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected void appendDetail(final StringBuffer buffer, final String fieldName, Object value) {
            if (value instanceof Annotation) {
                value = AnnotationUtils.toString((Annotation) value);
            }
            super.appendDetail(buffer, fieldName, value);
        }

    };

    /**
     * {@link AnnotationUtils} instances should NOT be constructed in
     * standard programming. Instead, the class should be used statically.
     *
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public AnnotationUtils() {
    }

    /**
     * Checks if two annotations are equal using the criteria for equality
     * presented in the {@link Annotation#equals(Object)} API docs.
     *
     * @param a1 the first Annotation to compare, {@code null} returns
     * {@code false} unless both are {@code null}
     * @param a2 the second Annotation to compare, {@code null} returns
     * {@code false} unless both are {@code null}
     * @return {@code true} if the two annotations are {@code equal} or both
     * {@code null}
     */
    public static boolean equals(final Annotation a1, final Annotation a2) {
        if (a1 == a2) {
            return true;
        }
        if (a1 == null || a2 == null) {
            return false;
        }
        final Class<? extends Annotation> type1 = a1.annotationType();
        final Class<? extends Annotation> type2 = a2.annotationType();
        Validate.notNull(type1, "Annotation %s with null annotationType()", a1);
        Validate.notNull(type2, "Annotation %s with null annotationType()", a2);
        if (!type1.equals(type2)) {
            return false;
        }
        try {
            for (final Method m : type1.getDeclaredMethods()) {
                if (m.getParameterTypes().length == 0
                        && isValidAnnotationMemberType(m.getReturnType())) {
                    final Object v1 = m.invoke(a1);
                    final Object v2 = m.invoke(a2);
                    if (!memberEquals(m.getReturnType(), v1, v2)) {
                        return false;
                    }
                }
            }
        } catch (final ReflectiveOperationException ex) {
            return false;
        }
        return true;
    }

    /**
     * Generate a hash code for the given annotation using the algorithm
     * presented in the {@link Annotation#hashCode()} API docs.
     *
     * @param a the Annotation for a hash code calculation is desired, not
     * {@code null}
     * @return the calculated hash code
     * @throws RuntimeException if an {@link Exception} is encountered during
     * annotation member access
     * @throws IllegalStateException if an annotation method invocation returns
     * {@code null}
     */
    public static int hashCode(final Annotation a) {
        int result = 0;
        final Class<? extends Annotation> type = a.annotationType();
        for (final Method m : type.getDeclaredMethods()) {
            try {
                final Object value = m.invoke(a);
                if (value == null) {
                    throw new IllegalStateException(String.format("Annotation method %s returned null", m));
                }
                result += hashMember(m.getName(), value);
            } catch (final ReflectiveOperationException ex) {
                throw new UncheckedException(ex);
            }
        }
        return result;
    }

    /**
     * Generate a string representation of an Annotation, as suggested by
     * {@link Annotation#toString()}.
     *
     * @param a the annotation of which a string representation is desired
     * @return the standard string representation of an annotation, not
     * {@code null}
     */
    public static String toString(final Annotation a) {
        final ToStringBuilder builder = new ToStringBuilder(a, TO_STRING_STYLE);
        for (final Method m : a.annotationType().getDeclaredMethods()) {
            if (m.getParameterTypes().length > 0) {
                continue; // wtf?
            }
            try {
                builder.append(m.getName(), m.invoke(a));
            } catch (final ReflectiveOperationException ex) {
                throw new UncheckedException(ex);
            }
        }
        return builder.build();
    }

    /**
     * Checks if the specified type is permitted as an annotation member.
     *
     * <p>The Java language specification only permits certain types to be used
     * in annotations. These include {@link String}, {@link Class}, primitive
     * types, {@link Annotation}, {@link Enum}, and single-dimensional arrays of
     * these types.</p>
     *
     * @param type the type to check, {@code null}
     * @return {@code true} if the type is a valid type to use in an annotation
     */
    public static boolean isValidAnnotationMemberType(Class<?> type) {
        if (type == null) {
            return false;
        }
        if (type.isArray()) {
            type = type.getComponentType();
        }
        return type.isPrimitive() || type.isEnum() || type.isAnnotation()
                || String.class.equals(type) || Class.class.equals(type);
    }

    //besides modularity, this has the advantage of autoboxing primitives:
    /**
     * Helper method for generating a hash code for a member of an annotation.
     *
     * @param name the name of the member
     * @param value the value of the member
     * @return a hash code for this member
     */
    private static int hashMember(final String name, final Object value) {
        final int part1 = name.hashCode() * 127;
        if (ObjectUtils.isArray(value)) {
            return part1 ^ arrayMemberHash(value.getClass().getComponentType(), value);
        }
        if (value instanceof Annotation) {
            return part1 ^ hashCode((Annotation) value);
        }
        return part1 ^ value.hashCode();
    }

    /**
     * Helper method for checking whether two objects of the given type are
     * equal. This method is used to compare the parameters of two annotation
     * instances.
     *
     * @param type the type of the objects to be compared
     * @param o1 the first object
     * @param o2 the second object
     * @return a flag whether these objects are equal
     */
    private static boolean memberEquals(final Class<?> type, final Object o1, final Object o2) {
        if (o1 == o2) {
            return true;
        }
        if (o1 == null || o2 == null) {
            return false;
        }
        if (type.isArray()) {
            return arrayMemberEquals(type.getComponentType(), o1, o2);
        }
        if (type.isAnnotation()) {
            return equals((Annotation) o1, (Annotation) o2);
        }
        return o1.equals(o2);
    }

    /**
     * Helper method for comparing two objects of an array type.
     *
     * @param componentType the component type of the array
     * @param o1 the first object
     * @param o2 the second object
     * @return a flag whether these objects are equal
     */
    private static boolean arrayMemberEquals(final Class<?> componentType, final Object o1, final Object o2) {
        if (componentType.isAnnotation()) {
            return annotationArrayMemberEquals((Annotation[]) o1, (Annotation[]) o2);
        }
        if (componentType.equals(Byte.TYPE)) {
            return Arrays.equals((byte[]) o1, (byte[]) o2);
        }
        if (componentType.equals(Short.TYPE)) {
            return Arrays.equals((short[]) o1, (short[]) o2);
        }
        if (componentType.equals(Integer.TYPE)) {
            return Arrays.equals((int[]) o1, (int[]) o2);
        }
        if (componentType.equals(Character.TYPE)) {
            return Arrays.equals((char[]) o1, (char[]) o2);
        }
        if (componentType.equals(Long.TYPE)) {
            return Arrays.equals((long[]) o1, (long[]) o2);
        }
        if (componentType.equals(Float.TYPE)) {
            return Arrays.equals((float[]) o1, (float[]) o2);
        }
        if (componentType.equals(Double.TYPE)) {
            return Arrays.equals((double[]) o1, (double[]) o2);
        }
        if (componentType.equals(Boolean.TYPE)) {
            return Arrays.equals((boolean[]) o1, (boolean[]) o2);
        }
        return Arrays.equals((Object[]) o1, (Object[]) o2);
    }

    /**
     * Helper method for comparing two arrays of annotations.
     *
     * @param a1 the first array
     * @param a2 the second array
     * @return a flag whether these arrays are equal
     */
    private static boolean annotationArrayMemberEquals(final Annotation[] a1, final Annotation[] a2) {
        if (a1.length != a2.length) {
            return false;
        }
        for (int i = 0; i < a1.length; i++) {
            if (!equals(a1[i], a2[i])) {
                return false;
            }
        }
        return true;
    }

    /**
     * Helper method for generating a hash code for an array.
     *
     * @param componentType the component type of the array
     * @param o the array
     * @return a hash code for the specified array
     */
    private static int arrayMemberHash(final Class<?> componentType, final Object o) {
        if (componentType.equals(Byte.TYPE)) {
            return Arrays.hashCode((byte[]) o);
        }
        if (componentType.equals(Short.TYPE)) {
            return Arrays.hashCode((short[]) o);
        }
        if (componentType.equals(Integer.TYPE)) {
            return Arrays.hashCode((int[]) o);
        }
        if (componentType.equals(Character.TYPE)) {
            return Arrays.hashCode((char[]) o);
        }
        if (componentType.equals(Long.TYPE)) {
            return Arrays.hashCode((long[]) o);
        }
        if (componentType.equals(Float.TYPE)) {
            return Arrays.hashCode((float[]) o);
        }
        if (componentType.equals(Double.TYPE)) {
            return Arrays.hashCode((double[]) o);
        }
        if (componentType.equals(Boolean.TYPE)) {
            return Arrays.hashCode((boolean[]) o);
        }
        return Arrays.hashCode((Object[]) o);
    }
}
