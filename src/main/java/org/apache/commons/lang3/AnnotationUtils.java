/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
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
        protected void appendDetail(final StringBuffer buffer, final String fieldName, Object value) {
            if (value instanceof Annotation) {
                value = AnnotationUtils.toString((Annotation) value);
            }
            super.appendDetail(buffer, fieldName, value);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected String getShortClassName(final Class<?> cls) {
            return ClassUtils.getAllInterfaces(cls).stream()
                    .filter(Annotation.class::isAssignableFrom)
                    .findFirst()
                    .map(iface -> "@" + iface.getName())
                    .orElse(StringUtils.EMPTY);
        }

    };

    // ---------------------- Helper dispatch methods ----------------------

    private static boolean isPrimitiveArrayType(Class<?> type) {
        return type.equals(Byte.TYPE) || type.equals(Short.TYPE)
                || type.equals(Integer.TYPE) || type.equals(Character.TYPE)
                || type.equals(Long.TYPE) || type.equals(Float.TYPE)
                || type.equals(Double.TYPE) || type.equals(Boolean.TYPE);
    }

    private static boolean primitiveArrayEquals(Class<?> type, Object o1, Object o2) {
        if (type.equals(Byte.TYPE)) return Arrays.equals((byte[]) o1, (byte[]) o2);
        if (type.equals(Short.TYPE)) return Arrays.equals((short[]) o1, (short[]) o2);
        if (type.equals(Integer.TYPE)) return Arrays.equals((int[]) o1, (int[]) o2);
        if (type.equals(Character.TYPE)) return Arrays.equals((char[]) o1, (char[]) o2);
        if (type.equals(Long.TYPE)) return Arrays.equals((long[]) o1, (long[]) o2);
        if (type.equals(Float.TYPE)) return Arrays.equals((float[]) o1, (float[]) o2);
        if (type.equals(Double.TYPE)) return Arrays.equals((double[]) o1, (double[]) o2);
        if (type.equals(Boolean.TYPE)) return Arrays.equals((boolean[]) o1, (boolean[]) o2);
        return false;
    }

    private static int primitiveArrayHash(Class<?> type, Object o) {
        if (type.equals(Byte.TYPE)) return Arrays.hashCode((byte[]) o);
        if (type.equals(Short.TYPE)) return Arrays.hashCode((short[]) o);
        if (type.equals(Integer.TYPE)) return Arrays.hashCode((int[]) o);
        if (type.equals(Character.TYPE)) return Arrays.hashCode((char[]) o);
        if (type.equals(Long.TYPE)) return Arrays.hashCode((long[]) o);
        if (type.equals(Float.TYPE)) return Arrays.hashCode((float[]) o);
        if (type.equals(Double.TYPE)) return Arrays.hashCode((double[]) o);
        if (type.equals(Boolean.TYPE)) return Arrays.hashCode((boolean[]) o);
        return 0;
    }

    /**
     * Helper method for comparing two arrays of annotations.
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
     * Helper method for comparing two objects of an array type.
     */
    private static boolean arrayMemberEquals(final Class<?> componentType, final Object o1, final Object o2) {
        if (componentType.isAnnotation()) {
            return annotationArrayMemberEquals((Annotation[]) o1, (Annotation[]) o2);
        }
        if (isPrimitiveArrayType(componentType)) {
            return primitiveArrayEquals(componentType, o1, o2);
        }
        return Arrays.equals((Object[]) o1, (Object[]) o2);
    }

    /**
     * Helper method for generating a hash code for an array.
     */
    private static int arrayMemberHash(final Class<?> componentType, final Object o) {
        if (isPrimitiveArrayType(componentType)) {
            return primitiveArrayHash(componentType, o);
        }
        return Arrays.hashCode((Object[]) o);
    }

    /**
     * Checks if two annotations are equal using the criteria for equality
     * presented in the {@link Annotation#equals(Object)} API docs.
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
     */
    public static int hashCode(final Annotation a) {
        int result = 0;
        final Class<? extends Annotation> type = a.annotationType();

        for (final Method m : type.getDeclaredMethods()) {
            try {
                final Object value = m.invoke(a);
                if (value == null) {
                    throw new IllegalStateException("Annotation method returned null: " + m);
                }
                result += hashMember(m.getName(), value);
            } catch (final ReflectiveOperationException ex) {
                throw new UncheckedException(ex);
            }
        }
        return result;
    }

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
     * Checks if the specified type is permitted as an annotation member.
     */
    public static boolean isValidAnnotationMemberType(Class<?> type) {
        if (type == null) {
            return false;
        }
        if (type.isArray()) {
            type = type.getComponentType();
        }
        return type.isPrimitive()
                || type.isEnum()
                || type.isAnnotation()
                || String.class.equals(type)
                || Class.class.equals(type);
    }

    /**
     * Helper method for checking whether two objects of the given type are
     * equal.
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
     * Generate a string representation of an Annotation, as suggested by
     * {@link Annotation#toString()}.
     */
    public static String toString(final Annotation a) {
        final ToStringBuilder builder = new ToStringBuilder(a, TO_STRING_STYLE);

        for (final Method m : a.annotationType().getDeclaredMethods()) {
            if (m.getParameterTypes().length > 0) {
                continue;
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
     * {@link AnnotationUtils} instances should NOT be constructed in
     * standard programming.
     */
    @Deprecated
    public AnnotationUtils() {
        // empty
    }
}