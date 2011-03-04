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
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * <p>Helper methods for working with {@link Annotation} instances.</p>
 * 
 * <p>This contains various utility methods that make working with annotations simpler.</p>
 * 
 * <p>#ThreadSafe#</p>
 * 
 * @since 3.0
 * @version $Id$
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
        protected String getShortClassName(java.lang.Class<?> cls) {
            Class<? extends Annotation> annotationType = null;
            for (Class<?> iface : ClassUtils.getAllInterfaces(cls)) {
                if (Annotation.class.isAssignableFrom(iface)) {
                    @SuppressWarnings("unchecked")
                    //because we just checked the assignability
                    Class<? extends Annotation> found = (Class<? extends Annotation>) iface;
                    annotationType = found;
                    break;
                }
            }
            return new StringBuilder(annotationType == null ? "" : annotationType.getName())
                    .insert(0, '@').toString();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected void appendDetail(StringBuffer buffer, String fieldName, Object value) {
            if (value instanceof Annotation) {
                value = AnnotationUtils.toString((Annotation) value);
            }
            super.appendDetail(buffer, fieldName, value);
        }

    };

    /**
     * <p><code>AnnotationUtils</code> instances should NOT be constructed in
     * standard programming. Instead, the class should be used statically.</p>
     * 
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public AnnotationUtils() {
    }

    //-----------------------------------------------------------------------
    /**
     * <p>Checks if two annotations are equal.</p>
     * 
     * <p>Dynamically created {@link Annotation} instances are always proxy
     * objects which cannot be depended upon to know how to implement
     * {@link Annotation#equals(Object)} correctly.</p>
     * 
     * @param a1 the first Annotation to compare, null returns false unless both are null
     * @param a2 the second Annotation to compare, null returns false unless both are null
     * @return true if the two annotations are equal or both null
     */
    public static boolean equals(Annotation a1, Annotation a2) {
        if (a1 == a2) {
            return true;
        }
        if (a1 == null || a2 == null) {
            return false;
        }
        Class<? extends Annotation> type = a1.annotationType();
        Class<? extends Annotation> type2 = a2.annotationType();
        Validate.notNull(type, "Annotation %s with null annotationType()", a1);
        Validate.notNull(type2, "Annotation %s with null annotationType()", a2);
        if (!type.equals(type2)) {
            return false;
        }
        try {
            for (Method m : type.getDeclaredMethods()) {
                if (m.getParameterTypes().length == 0
                        && isValidAnnotationMemberType(m.getReturnType())) {
                    Object v1 = m.invoke(a1);
                    Object v2 = m.invoke(a2);
                    if (!memberEquals(m.getReturnType(), v1, v2)) {
                        return false;
                    }
                }
            }
        } catch (IllegalAccessException ex) {
            return false;
        } catch (InvocationTargetException ex) {
            return false;
        }
        return true;
    }

    /**
     * <p>Generate a hash code for the given annotation.</p>
     * 
     * <p>Dynamically created {@link Annotation} instances are always proxy
     * objects which cannot be depended upon to know how to implement
     * {@link Annotation#hashCode()} correctly.</p>
     * 
     * @param a the Annotation for a hash code calculation is desired, not null
     * @return the calculated hash code
     * @throws IllegalAccessException if thrown during annotation access
     * @throws InvocationTargetException if thrown during annotation access
     */
    public static int hashCode(Annotation a) {
        int result = 0;
        Class<? extends Annotation> type = a.annotationType();
        for (Method m : type.getDeclaredMethods()) {
            try {
                Object value = m.invoke(a);
                if (value == null) {
                    throw new IllegalStateException(
                            String.format("Annotation method %s returned null", m));
                }
                result += hashMember(m.getName(), value);
            } catch (RuntimeException ex) {
                throw ex;
            } catch (Exception ex) {
                throw new RuntimeException(ex);
            }
        }
        return result;
    }

    /**
     * <p>Generate a string representation of an Annotation, as suggested by
     * {@link Annotation#toString()}.</p>
     * 
     * @param a the annotation of which a string representation is desired
     * @return the standard string representation of an annotation, not null
     */
    public static String toString(final Annotation a) {
        ToStringBuilder builder = new ToStringBuilder(a, TO_STRING_STYLE);
        for (Method m : a.annotationType().getDeclaredMethods()) {
            if (m.getParameterTypes().length > 0) {
                continue; //wtf?
            }
            try {
                builder.append(m.getName(), m.invoke(a));
            } catch (RuntimeException ex) {
                throw ex;
            } catch (Exception ex) {
                throw new RuntimeException(ex);
            }
        }
        return builder.build();
    }

    /**
     * <p>Checks if the specified type is permitted as an annotation member.</p>
     * 
     * <p>The Java language specification only permits certain types to be used
     * in annotations. These include {@link String}, {@link Class}, primitive types,
     * {@link Annotation}, {@link Enum}, and arrays of these types.</p>
     * 
     * @param type the type to check, null returns false
     * @return true if the type is a valid type to use in an annotation
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
    private static int hashMember(String name, Object value)
            throws IllegalAccessException, InvocationTargetException {
        int part1 = name.hashCode() * 127;
        if (value.getClass().isArray()) {
            return part1 ^ arrayMemberHash(value.getClass().getComponentType(), value);
        }
        if (value instanceof Annotation) {
            return part1 ^ hashCode((Annotation) value);
        }
        return part1 ^ value.hashCode();
    }

    private static boolean memberEquals(Class<?> type, Object o1, Object o2) {
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

    private static boolean arrayMemberEquals(Class<?> componentType, Object o1, Object o2) {
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

    private static boolean annotationArrayMemberEquals(Annotation[] a1, Annotation[] a2) {
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

    private static int arrayMemberHash(Class<?> componentType, Object o) {
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
