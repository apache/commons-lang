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
package org.apache.commons.lang3.reflect;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.JavaVersion;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.Validate;

/**
 * Utilities for working with {@link Field}s by reflection. Adapted and refactored from the dormant [reflect] Commons
 * sandbox component.
 * <p>
 * The ability is provided to break the scoping restrictions coded by the programmer. This can allow fields to be
 * changed that shouldn't be. This facility should be used with care.
 * </p>
 * @since 2.5
 */
public class FieldUtils {

    /**
     * {@link FieldUtils} instances should NOT be constructed in standard programming.
     * <p>
     * This constructor is {@code public} to permit tools that require a JavaBean instance to operate.
     * </p>
     */
    public FieldUtils() {
    }

    /**
     * Gets an accessible {@link Field} by name respecting scope. Superclasses/interfaces will be considered.
     *
     * @param cls
     *            the {@link Class} to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @return the Field object
     * @throws IllegalArgumentException
     *             if the class is {@code null}, or the field name is blank or empty
     */
    public static Field getField(final Class<?> cls, final String fieldName) {
        return MemberUtils.setAccessibleWorkaround(getField(cls, fieldName, false));
    }

    /**
     * Gets an accessible {@link Field} by name, breaking scope if requested. Superclasses/interfaces will be
     * considered.
     *
     * @param cls
     *            the {@link Class} to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match {@code public} fields.
     * @return the Field object
     * @throws NullPointerException if the class is {@code null}
     * @throws IllegalArgumentException if the field name is blank or empty or is matched at multiple places
     * in the inheritance hierarchy
     */
    public static Field getField(final Class<?> cls, final String fieldName, final boolean forceAccess) {
        Objects.requireNonNull(cls, "cls");
        Validate.isTrue(StringUtils.isNotBlank(fieldName), "The field name must not be blank/empty");
        // FIXME is this workaround still needed? lang requires Java 6
        // Sun Java 1.3 has a bugged implementation of getField hence we write the
        // code ourselves

        // getField() will return the Field object with the declaring class
        // set correctly to the class that declares the field. Thus requesting the
        // field on a subclass will return the field from the superclass.
        //
        // priority order for lookup:
        // searchclass private/protected/package/public
        // superclass protected/package/public
        // private/different package blocks access to further superclasses
        // implementedinterface public

        // check up the superclass hierarchy
        for (Class<?> acls = cls; acls != null; acls = acls.getSuperclass()) {
            try {
                final Field field = acls.getDeclaredField(fieldName);
                // getDeclaredField checks for non-public scopes as well
                // and it returns accurate results
                if (!MemberUtils.isPublic(field)) {
                    if (!forceAccess) {
                        continue;
                    }
                    field.setAccessible(true);
                }
                return field;
            } catch (final NoSuchFieldException ignored) {
                // ignore
            }
        }
        // check the public interface case. This must be manually searched for
        // incase there is a public supersuperclass field hidden by a private/package
        // superclass field.
        Field match = null;
        for (final Class<?> class1 : ClassUtils.getAllInterfaces(cls)) {
            try {
                final Field test = class1.getField(fieldName);
                Validate.isTrue(match == null, "Reference to field %s is ambiguous relative to %s"
                        + "; a matching field exists on two or more implemented interfaces.", fieldName, cls);
                match = test;
            } catch (final NoSuchFieldException ignored) {
                // ignore
            }
        }
        return match;
    }

    /**
     * Gets an accessible {@link Field} by name respecting scope. Only the specified class will be considered.
     *
     * @param cls
     *            the {@link Class} to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @return the Field object
     * @throws IllegalArgumentException
     *             if the class is {@code null}, or the field name is blank or empty
     */
    public static Field getDeclaredField(final Class<?> cls, final String fieldName) {
        return getDeclaredField(cls, fieldName, false);
    }

    /**
     * Gets an accessible {@link Field} by name, breaking scope if requested. Only the specified class will be
     * considered.
     *
     * @param cls
     *            the {@link Class} to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match {@code public} fields.
     * @return the Field object
     * @throws IllegalArgumentException
     *             if the class is {@code null}, or the field name is blank or empty
     */
    public static Field getDeclaredField(final Class<?> cls, final String fieldName, final boolean forceAccess) {
        Objects.requireNonNull(cls, "cls");
        Validate.isTrue(StringUtils.isNotBlank(fieldName), "The field name must not be blank/empty");
        try {
            // only consider the specified class by using getDeclaredField()
            final Field field = cls.getDeclaredField(fieldName);
            if (!MemberUtils.isAccessible(field)) {
                if (!forceAccess) {
                    return null;
                }
                field.setAccessible(true);
            }
            return field;
        } catch (final NoSuchFieldException ignored) {
            // ignore
        }
        return null;
    }

    /**
     * Gets all fields of the given class and its parents (if any).
     *
     * @param cls
     *            the {@link Class} to query
     * @return an array of Fields (possibly empty).
     * @throws IllegalArgumentException
     *             if the class is {@code null}
     * @since 3.2
     */
    public static Field[] getAllFields(final Class<?> cls) {
        return getAllFieldsList(cls).toArray(ArrayUtils.EMPTY_FIELD_ARRAY);
    }

    /**
     * Gets all fields of the given class and its parents (if any).
     *
     * @param cls
     *            the {@link Class} to query
     * @return a list of Fields (possibly empty).
     * @throws IllegalArgumentException
     *             if the class is {@code null}
     * @since 3.2
     */
    public static List<Field> getAllFieldsList(final Class<?> cls) {
        Objects.requireNonNull(cls, "cls");
        final List<Field> allFields = new ArrayList<>();
        Class<?> currentClass = cls;
        while (currentClass != null) {
            final Field[] declaredFields = currentClass.getDeclaredFields();
            Collections.addAll(allFields, declaredFields);
            currentClass = currentClass.getSuperclass();
        }
        return allFields;
    }

    /**
     * Gets all fields of the given class and its parents (if any) that are annotated with the given annotation.
     * @param cls
     *            the {@link Class} to query
     * @param annotationCls
     *            the {@link Annotation} that must be present on a field to be matched
     * @return an array of Fields (possibly empty).
     * @throws IllegalArgumentException
     *            if the class or annotation are {@code null}
     * @since 3.4
     */
    public static Field[] getFieldsWithAnnotation(final Class<?> cls, final Class<? extends Annotation> annotationCls) {
        return getFieldsListWithAnnotation(cls, annotationCls).toArray(ArrayUtils.EMPTY_FIELD_ARRAY);
    }

    /**
     * Gets all fields of the given class and its parents (if any) that are annotated with the given annotation.
     * @param cls
     *            the {@link Class} to query
     * @param annotationCls
     *            the {@link Annotation} that must be present on a field to be matched
     * @return a list of Fields (possibly empty).
     * @throws IllegalArgumentException
     *            if the class or annotation are {@code null}
     * @since 3.4
     */
    public static List<Field> getFieldsListWithAnnotation(final Class<?> cls, final Class<? extends Annotation> annotationCls) {
        Objects.requireNonNull(annotationCls, "annotationCls");
        return getAllFieldsList(cls).stream().filter(field -> field.getAnnotation(annotationCls) != null).collect(Collectors.toList());
    }

    /**
     * Reads an accessible {@code static} {@link Field}.
     *
     * @param field
     *            to read
     * @return the field value
     * @throws IllegalArgumentException
     *             if the field is {@code null}, or not {@code static}
     * @throws IllegalAccessException
     *             if the field is not accessible
     */
    public static Object readStaticField(final Field field) throws IllegalAccessException {
        return readStaticField(field, false);
    }

    /**
     * Reads a static {@link Field}.
     *
     * @param field
     *            to read
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method.
     * @return the field value
     * @throws IllegalArgumentException
     *             if the field is {@code null} or not {@code static}
     * @throws IllegalAccessException
     *             if the field is not made accessible
     */
    public static Object readStaticField(final Field field, final boolean forceAccess) throws IllegalAccessException {
        Objects.requireNonNull(field, "field");
        Validate.isTrue(MemberUtils.isStatic(field), "The field '%s' is not static", field.getName());
        return readField(field, (Object) null, forceAccess);
    }

    /**
     * Reads the named {@code public static} {@link Field}. Superclasses will be considered.
     *
     * @param cls
     *            the {@link Class} to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException
     *             if the class is {@code null}, or the field name is blank or empty, is not {@code static}, or could
     *             not be found
     * @throws IllegalAccessException
     *             if the field is not accessible
     */
    public static Object readStaticField(final Class<?> cls, final String fieldName) throws IllegalAccessException {
        return readStaticField(cls, fieldName, false);
    }

    /**
     * Reads the named {@code static} {@link Field}. Superclasses will be considered.
     *
     * @param cls
     *            the {@link Class} to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match {@code public} fields.
     * @return the Field object
     * @throws IllegalArgumentException
     *             if the class is {@code null}, or the field name is blank or empty, is not {@code static}, or could
     *             not be found
     * @throws IllegalAccessException
     *             if the field is not made accessible
     */
    public static Object readStaticField(final Class<?> cls, final String fieldName, final boolean forceAccess) throws IllegalAccessException {
        final Field field = getField(cls, fieldName, forceAccess);
        Validate.notNull(field, "Cannot locate field '%s' on %s", fieldName, cls);
        // already forced access above, don't repeat it here:
        return readStaticField(field, false);
    }

    /**
     * Gets the value of a {@code static} {@link Field} by name. The field must be {@code public}. Only the specified
     * class will be considered.
     *
     * @param cls
     *            the {@link Class} to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException
     *             if the class is {@code null}, or the field name is blank or empty, is not {@code static}, or could
     *             not be found
     * @throws IllegalAccessException
     *             if the field is not accessible
     */
    public static Object readDeclaredStaticField(final Class<?> cls, final String fieldName) throws IllegalAccessException {
        return readDeclaredStaticField(cls, fieldName, false);
    }

    /**
     * Gets the value of a {@code static} {@link Field} by name. Only the specified class will be considered.
     *
     * @param cls
     *            the {@link Class} to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match {@code public} fields.
     * @return the Field object
     * @throws IllegalArgumentException
     *             if the class is {@code null}, or the field name is blank or empty, is not {@code static}, or could
     *             not be found
     * @throws IllegalAccessException
     *             if the field is not made accessible
     */
    public static Object readDeclaredStaticField(final Class<?> cls, final String fieldName, final boolean forceAccess) throws IllegalAccessException {
        final Field field = getDeclaredField(cls, fieldName, forceAccess);
        Validate.notNull(field, "Cannot locate declared field %s.%s", cls.getName(), fieldName);
        // already forced access above, don't repeat it here:
        return readStaticField(field, false);
    }

    /**
     * Reads an accessible {@link Field}.
     *
     * @param field
     *            the field to use
     * @param target
     *            the object to call on, may be {@code null} for {@code static} fields
     * @return the field value
     * @throws IllegalArgumentException
     *             if the field is {@code null}
     * @throws IllegalAccessException
     *             if the field is not accessible
     */
    public static Object readField(final Field field, final Object target) throws IllegalAccessException {
        return readField(field, target, false);
    }

    /**
     * Reads a {@link Field}.
     *
     * @param field
     *            the field to use
     * @param target
     *            the object to call on, may be {@code null} for {@code static} fields
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method.
     * @return the field value
     * @throws IllegalArgumentException
     *             if the field is {@code null}
     * @throws IllegalAccessException
     *             if the field is not made accessible
     */
    public static Object readField(final Field field, final Object target, final boolean forceAccess) throws IllegalAccessException {
        Objects.requireNonNull(field, "field");
        if (forceAccess && !field.isAccessible()) {
            field.setAccessible(true);
        } else {
            MemberUtils.setAccessibleWorkaround(field);
        }
        return field.get(target);
    }

    /**
     * Reads the named {@code public} {@link Field}. Superclasses will be considered.
     *
     * @param target
     *            the object to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException
     *             if the class is {@code null}, or the field name is blank or empty or could not be found
     * @throws IllegalAccessException
     *             if the named field is not {@code public}
     */
    public static Object readField(final Object target, final String fieldName) throws IllegalAccessException {
        return readField(target, fieldName, false);
    }

    /**
     * Reads the named {@link Field}. Superclasses will be considered.
     *
     * @param target
     *            the object to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match {@code public} fields.
     * @return the field value
     * @throws IllegalArgumentException
     *             if {@code target} is {@code null}, or the field name is blank or empty or could not be found
     * @throws IllegalAccessException
     *             if the named field is not made accessible
     */
    public static Object readField(final Object target, final String fieldName, final boolean forceAccess) throws IllegalAccessException {
        Objects.requireNonNull(target, "target");
        final Class<?> cls = target.getClass();
        final Field field = getField(cls, fieldName, forceAccess);
        Validate.isTrue(field != null, "Cannot locate field %s on %s", fieldName, cls);
        // already forced access above, don't repeat it here:
        return readField(field, target, false);
    }

    /**
     * Reads the named {@code public} {@link Field}. Only the class of the specified object will be considered.
     *
     * @param target
     *            the object to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @return the value of the field
     * @throws IllegalArgumentException
     *             if {@code target} is {@code null}, or the field name is blank or empty or could not be found
     * @throws IllegalAccessException
     *             if the named field is not {@code public}
     */
    public static Object readDeclaredField(final Object target, final String fieldName) throws IllegalAccessException {
        return readDeclaredField(target, fieldName, false);
    }

    /**
     * Gets a {@link Field} value by name. Only the class of the specified object will be considered.
     *
     * @param target
     *            the object to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match public fields.
     * @return the Field object
     * @throws IllegalArgumentException
     *             if {@code target} is {@code null}, or the field name is blank or empty or could not be found
     * @throws IllegalAccessException
     *             if the field is not made accessible
     */
    public static Object readDeclaredField(final Object target, final String fieldName, final boolean forceAccess) throws IllegalAccessException {
        Objects.requireNonNull(target, "target");
        final Class<?> cls = target.getClass();
        final Field field = getDeclaredField(cls, fieldName, forceAccess);
        Validate.isTrue(field != null, "Cannot locate declared field %s.%s", cls, fieldName);
        // already forced access above, don't repeat it here:
        return readField(field, target, false);
    }

    /**
     * Writes a {@code public static} {@link Field}.
     *
     * @param field
     *            to write
     * @param value
     *            to set
     * @throws IllegalArgumentException
     *             if the field is {@code null} or not {@code static}, or {@code value} is not assignable
     * @throws IllegalAccessException
     *             if the field is not {@code public} or is {@code final}
     */
    public static void writeStaticField(final Field field, final Object value) throws IllegalAccessException {
        writeStaticField(field, value, false);
    }

    /**
     * Writes a static {@link Field}.
     *
     * @param field
     *            to write
     * @param value
     *            to set
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match {@code public} fields.
     * @throws IllegalArgumentException
     *             if the field is {@code null} or not {@code static}, or {@code value} is not assignable
     * @throws IllegalAccessException
     *             if the field is not made accessible or is {@code final}
     */
    public static void writeStaticField(final Field field, final Object value, final boolean forceAccess) throws IllegalAccessException {
        Objects.requireNonNull(field, "field");
        Validate.isTrue(MemberUtils.isStatic(field), "The field %s.%s is not static", field.getDeclaringClass().getName(),
                field.getName());
        writeField(field, (Object) null, value, forceAccess);
    }

    /**
     * Writes a named {@code public static} {@link Field}. Superclasses will be considered.
     *
     * @param cls
     *            {@link Class} on which the field is to be found
     * @param fieldName
     *            to write
     * @param value
     *            to set
     * @throws IllegalArgumentException
     *             if {@code cls} is {@code null}, the field name is blank or empty, the field cannot be located or is
     *             not {@code static}, or {@code value} is not assignable
     * @throws IllegalAccessException
     *             if the field is not {@code public} or is {@code final}
     */
    public static void writeStaticField(final Class<?> cls, final String fieldName, final Object value) throws IllegalAccessException {
        writeStaticField(cls, fieldName, value, false);
    }

    /**
     * Writes a named {@code static} {@link Field}. Superclasses will be considered.
     *
     * @param cls
     *            {@link Class} on which the field is to be found
     * @param fieldName
     *            to write
     * @param value
     *            to set
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match {@code public} fields.
     * @throws IllegalArgumentException
     *             if {@code cls} is {@code null}, the field name is blank or empty, the field cannot be located or is
     *             not {@code static}, or {@code value} is not assignable
     * @throws IllegalAccessException
     *             if the field is not made accessible or is {@code final}
     */
    public static void writeStaticField(final Class<?> cls, final String fieldName, final Object value, final boolean forceAccess)
            throws IllegalAccessException {
        final Field field = getField(cls, fieldName, forceAccess);
        Validate.notNull(field, "Cannot locate field %s on %s", fieldName, cls);
        // already forced access above, don't repeat it here:
        writeStaticField(field, value, false);
    }

    /**
     * Writes a named {@code public static} {@link Field}. Only the specified class will be considered.
     *
     * @param cls
     *            {@link Class} on which the field is to be found
     * @param fieldName
     *            to write
     * @param value
     *            to set
     * @throws IllegalArgumentException
     *             if {@code cls} is {@code null}, the field name is blank or empty, the field cannot be located or is
     *             not {@code static}, or {@code value} is not assignable
     * @throws IllegalAccessException
     *             if the field is not {@code public} or is {@code final}
     */
    public static void writeDeclaredStaticField(final Class<?> cls, final String fieldName, final Object value) throws IllegalAccessException {
        writeDeclaredStaticField(cls, fieldName, value, false);
    }

    /**
     * Writes a named {@code static} {@link Field}. Only the specified class will be considered.
     *
     * @param cls
     *            {@link Class} on which the field is to be found
     * @param fieldName
     *            to write
     * @param value
     *            to set
     * @param forceAccess
     *            whether to break scope restrictions using the {@code AccessibleObject#setAccessible(boolean)} method.
     *            {@code false} will only match {@code public} fields.
     * @throws IllegalArgumentException
     *             if {@code cls} is {@code null}, the field name is blank or empty, the field cannot be located or is
     *             not {@code static}, or {@code value} is not assignable
     * @throws IllegalAccessException
     *             if the field is not made accessible or is {@code final}
     */
    public static void writeDeclaredStaticField(final Class<?> cls, final String fieldName, final Object value, final boolean forceAccess)
            throws IllegalAccessException {
        final Field field = getDeclaredField(cls, fieldName, forceAccess);
        Validate.notNull(field, "Cannot locate declared field %s.%s", cls.getName(), fieldName);
        // already forced access above, don't repeat it here:
        writeField(field, (Object) null, value, false);
    }

    /**
     * Writes an accessible {@link Field}.
     *
     * @param field
     *            to write
     * @param target
     *            the object to call on, may be {@code null} for {@code static} fields
     * @param value
     *            to set
     * @throws IllegalAccessException
     *             if the field or target is {@code null}, the field is not accessible or is {@code final}, or
     *             {@code value} is not assignable
     */
    public static void writeField(final Field field, final Object target, final Object value) throws IllegalAccessException {
        writeField(field, target, value, false);
    }

    /**
     * Writes a {@link Field}.
     *
     * @param field
     *            to write
     * @param target
     *            the object to call on, may be {@code null} for {@code static} fields
     * @param value
     *            to set
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match {@code public} fields.
     * @throws IllegalArgumentException
     *             if the field is {@code null} or {@code value} is not assignable
     * @throws IllegalAccessException
     *             if the field is not made accessible or is {@code final}
     */
    public static void writeField(final Field field, final Object target, final Object value, final boolean forceAccess)
            throws IllegalAccessException {
        Objects.requireNonNull(field, "field");
        if (forceAccess && !field.isAccessible()) {
            field.setAccessible(true);
        } else {
            MemberUtils.setAccessibleWorkaround(field);
        }
        field.set(target, value);
    }

    /**
     * Removes the final modifier from a {@link Field}.
     *
     * @param field
     *            to remove the final modifier
     * @throws IllegalArgumentException
     *             if the field is {@code null}
     * @since 3.2
     */
    public static void removeFinalModifier(final Field field) {
        removeFinalModifier(field, true);
    }

    /**
     * Removes the final modifier from a {@link Field}.
     *
     * @param field
     *            to remove the final modifier
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match {@code public} fields.
     * @throws IllegalArgumentException
     *             if the field is {@code null}
     * @deprecated As of Java 12, we can no longer drop the {@code final} modifier, thus
     *             rendering this method obsolete. The JDK discussion about this change can be found
     *             here: https://mail.openjdk.java.net/pipermail/core-libs-dev/2018-November/056486.html
     * @since 3.3
     */
    @Deprecated
    public static void removeFinalModifier(final Field field, final boolean forceAccess) {
        Objects.requireNonNull(field, "field");

        try {
            if (Modifier.isFinal(field.getModifiers())) {
                // Do all JREs implement Field with a private ivar called "modifiers"?
                final Field modifiersField = Field.class.getDeclaredField("modifiers");
                final boolean doForceAccess = forceAccess && !modifiersField.isAccessible();
                if (doForceAccess) {
                    modifiersField.setAccessible(true);
                }
                try {
                    modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
                } finally {
                    if (doForceAccess) {
                        modifiersField.setAccessible(false);
                    }
                }
            }
        } catch (final NoSuchFieldException | IllegalAccessException e) {
            if (SystemUtils.isJavaVersionAtLeast(JavaVersion.JAVA_12)) {
              throw new UnsupportedOperationException(
                  "In java 12+ final cannot be removed.",
                  e
              );
            }
            // else no exception is thrown because we can modify final.
        }
    }

    /**
     * Writes a {@code public} {@link Field}. Superclasses will be considered.
     *
     * @param target
     *            the object to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @param value
     *            to set
     * @throws IllegalArgumentException
     *             if {@code target} is {@code null}, {@code fieldName} is blank or empty or could not be found, or
     *             {@code value} is not assignable
     * @throws IllegalAccessException
     *             if the field is not accessible
     */
    public static void writeField(final Object target, final String fieldName, final Object value) throws IllegalAccessException {
        writeField(target, fieldName, value, false);
    }

    /**
     * Writes a {@link Field}. Superclasses will be considered.
     *
     * @param target
     *            the object to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @param value
     *            to set
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match {@code public} fields.
     * @throws IllegalArgumentException
     *             if {@code target} is {@code null}, {@code fieldName} is blank or empty or could not be found, or
     *             {@code value} is not assignable
     * @throws IllegalAccessException
     *             if the field is not made accessible
     */
    public static void writeField(final Object target, final String fieldName, final Object value, final boolean forceAccess)
            throws IllegalAccessException {
        Objects.requireNonNull(target, "target");
        final Class<?> cls = target.getClass();
        final Field field = getField(cls, fieldName, forceAccess);
        Validate.isTrue(field != null, "Cannot locate declared field %s.%s", cls.getName(), fieldName);
        // already forced access above, don't repeat it here:
        writeField(field, target, value, false);
    }

    /**
     * Writes a {@code public} {@link Field}. Only the specified class will be considered.
     *
     * @param target
     *            the object to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @param value
     *            to set
     * @throws IllegalArgumentException
     *             if {@code target} is {@code null}, {@code fieldName} is blank or empty or could not be found, or
     *             {@code value} is not assignable
     * @throws IllegalAccessException
     *             if the field is not made accessible
     */
    public static void writeDeclaredField(final Object target, final String fieldName, final Object value) throws IllegalAccessException {
        writeDeclaredField(target, fieldName, value, false);
    }

    /**
     * Writes a {@code public} {@link Field}. Only the specified class will be considered.
     *
     * @param target
     *            the object to reflect, must not be {@code null}
     * @param fieldName
     *            the field name to obtain
     * @param value
     *            to set
     * @param forceAccess
     *            whether to break scope restrictions using the
     *            {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} method. {@code false} will only
     *            match {@code public} fields.
     * @throws IllegalArgumentException
     *             if {@code target} is {@code null}, {@code fieldName} is blank or empty or could not be found, or
     *             {@code value} is not assignable
     * @throws IllegalAccessException
     *             if the field is not made accessible
     */
    public static void writeDeclaredField(final Object target, final String fieldName, final Object value, final boolean forceAccess)
            throws IllegalAccessException {
        Objects.requireNonNull(target, "target");
        final Class<?> cls = target.getClass();
        final Field field = getDeclaredField(cls, fieldName, forceAccess);
        Validate.isTrue(field != null, "Cannot locate declared field %s.%s", cls.getName(), fieldName);
        // already forced access above, don't repeat it here:
        writeField(field, target, value, false);
    }
}
