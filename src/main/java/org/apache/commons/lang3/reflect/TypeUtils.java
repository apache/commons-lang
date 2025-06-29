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
package org.apache.commons.lang3.reflect;

import java.lang.reflect.Array;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.AppendableJoiner;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.Validate;
import org.apache.commons.lang3.builder.Builder;

/**
 * Utility methods focusing on type inspection, particularly with regard to generics.
 *
 * @since 3.0
 */
public class TypeUtils {

    /**
     * GenericArrayType implementation class.
     */
    private static final class GenericArrayTypeImpl implements GenericArrayType {
        private final Type componentType;

        /**
         * Constructor
         *
         * @param componentType of this array type
         */
        private GenericArrayTypeImpl(final Type componentType) {
            this.componentType = componentType;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean equals(final Object obj) {
            return obj == this || obj instanceof GenericArrayType && TypeUtils.equals(this, (GenericArrayType) obj);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Type getGenericComponentType() {
            return componentType;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int hashCode() {
            int result = 67 << 4;
            result |= componentType.hashCode();
            return result;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String toString() {
            return TypeUtils.toString(this);
        }
    }

    /**
     * ParameterizedType implementation class.
     */
    private static final class ParameterizedTypeImpl implements ParameterizedType {
        private final Class<?> raw;
        private final Type useOwner;
        private final Type[] typeArguments;

        /**
         * Constructor
         *
         * @param rawClass      type
         * @param useOwner      owner type to use, if any
         * @param typeArguments formal type arguments
         */
        private ParameterizedTypeImpl(final Class<?> rawClass, final Type useOwner, final Type[] typeArguments) {
            this.raw = rawClass;
            this.useOwner = useOwner;
            this.typeArguments = Arrays.copyOf(typeArguments, typeArguments.length, Type[].class);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean equals(final Object obj) {
            return obj == this || obj instanceof ParameterizedType && TypeUtils.equals(this, (ParameterizedType) obj);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Type[] getActualTypeArguments() {
            return typeArguments.clone();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Type getOwnerType() {
            return useOwner;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Type getRawType() {
            return raw;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int hashCode() {
            int result = 71 << 4;
            result |= raw.hashCode();
            result <<= 4;
            result |= Objects.hashCode(useOwner);
            result <<= 8;
            result |= Arrays.hashCode(typeArguments);
            return result;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String toString() {
            return TypeUtils.toString(this);
        }
    }

    /**
     * {@link WildcardType} builder.
     *
     * @since 3.2
     */
    public static class WildcardTypeBuilder implements Builder<WildcardType> {
        private Type[] upperBounds;

        private Type[] lowerBounds;

        /**
         * Constructor
         */
        private WildcardTypeBuilder() {
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public WildcardType build() {
            return new WildcardTypeImpl(upperBounds, lowerBounds);
        }

        /**
         * Specify lower bounds of the wildcard type to build.
         *
         * @param bounds to set
         * @return {@code this}
         */
        public WildcardTypeBuilder withLowerBounds(final Type... bounds) {
            this.lowerBounds = bounds;
            return this;
        }

        /**
         * Specify upper bounds of the wildcard type to build.
         *
         * @param bounds to set
         * @return {@code this}
         */
        public WildcardTypeBuilder withUpperBounds(final Type... bounds) {
            this.upperBounds = bounds;
            return this;
        }
    }

    /**
     * WildcardType implementation class.
     */
    private static final class WildcardTypeImpl implements WildcardType {
        private final Type[] upperBounds;
        private final Type[] lowerBounds;

        /**
         * Constructor
         *
         * @param upperBounds of this type
         * @param lowerBounds of this type
         */
        private WildcardTypeImpl(final Type[] upperBounds, final Type[] lowerBounds) {
            this.upperBounds = ObjectUtils.getIfNull(upperBounds, ArrayUtils.EMPTY_TYPE_ARRAY);
            this.lowerBounds = ObjectUtils.getIfNull(lowerBounds, ArrayUtils.EMPTY_TYPE_ARRAY);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean equals(final Object obj) {
            return obj == this || obj instanceof WildcardType && TypeUtils.equals(this, (WildcardType) obj);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Type[] getLowerBounds() {
            return lowerBounds.clone();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public Type[] getUpperBounds() {
            return upperBounds.clone();
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int hashCode() {
            int result = 73 << 8;
            result |= Arrays.hashCode(upperBounds);
            result <<= 8;
            result |= Arrays.hashCode(lowerBounds);
            return result;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public String toString() {
            return TypeUtils.toString(this);
        }
    }

    /**
     * Ampersand sign joiner.
     */
    // @formatter:off
    private static final AppendableJoiner<Type> AMP_JOINER = AppendableJoiner.<Type>builder()
            .setDelimiter(" & ")
            .setElementAppender((a, e) -> a.append(toString(e)))
            .get();
    // @formatter:on

    /**
     * Method classToString joiner.
     */
    // @formatter:off
    private static final AppendableJoiner<TypeVariable<Class<?>>> CTJ_JOINER = AppendableJoiner.<TypeVariable<Class<?>>>builder()
        .setDelimiter(", ")
        .setElementAppender((a, e) -> a.append(anyToString(e)))
        .get();
    // @formatter:on

    /**
     * Greater than and lesser than sign joiner.
     */
    // @formatter:off
    private static final AppendableJoiner<Object> GT_JOINER = AppendableJoiner.builder()
            .setPrefix("<")
            .setSuffix(">")
            .setDelimiter(", ")
            .setElementAppender((a, e) -> a.append(anyToString(e)))
            .get();
    // @formatter:on

    /**
     * A wildcard instance matching {@code ?}.
     *
     * @since 3.2
     */
    public static final WildcardType WILDCARD_ALL = wildcardType().withUpperBounds(Object.class).build();

    private static <T> String anyToString(final T object) {
        return object instanceof Type ? toString((Type) object) : object.toString();
    }

    private static void appendRecursiveTypes(final StringBuilder builder, final int[] recursiveTypeIndexes, final Type[] argumentTypes) {
        for (int i = 0; i < recursiveTypeIndexes.length; i++) {
            // toString() or SO
            GT_JOINER.join(builder, argumentTypes[i].toString());
        }
        final Type[] argumentsFiltered = ArrayUtils.removeAll(argumentTypes, recursiveTypeIndexes);
        if (argumentsFiltered.length > 0) {
            GT_JOINER.join(builder, (Object[]) argumentsFiltered);
        }
    }

    /**
     * Formats a {@link Class} as a {@link String}.
     *
     * @param cls {@link Class} to format
     * @return String
     */
    private static <T> String classToString(final Class<T> cls) {
        if (cls.isArray()) {
            return toString(cls.getComponentType()) + "[]";
        }
        if (isCyclical(cls)) {
            return cls.getSimpleName() + "(cycle)";
        }
        final StringBuilder buf = new StringBuilder();
        if (cls.getEnclosingClass() != null) {
            buf.append(classToString(cls.getEnclosingClass())).append('.').append(cls.getSimpleName());
        } else {
            buf.append(cls.getName());
        }
        if (cls.getTypeParameters().length > 0) {
            CTJ_JOINER.join(buf, (TypeVariable[]) cls.getTypeParameters());
        }
        return buf.toString();
    }

    /**
     * Tests, recursively, whether any of the type parameters associated with {@code type} are bound to variables.
     *
     * @param type the type to check for type variables
     * @return boolean
     * @since 3.2
     */
    public static boolean containsTypeVariables(final Type type) {
        if (type instanceof TypeVariable<?>) {
            return true;
        }
        if (type instanceof Class<?>) {
            return ((Class<?>) type).getTypeParameters().length > 0;
        }
        if (type instanceof ParameterizedType) {
            for (final Type arg : ((ParameterizedType) type).getActualTypeArguments()) {
                if (containsTypeVariables(arg)) {
                    return true;
                }
            }
            return false;
        }
        if (type instanceof WildcardType) {
            final WildcardType wild = (WildcardType) type;
            return containsTypeVariables(getImplicitLowerBounds(wild)[0]) || containsTypeVariables(getImplicitUpperBounds(wild)[0]);
        }
        if (type instanceof GenericArrayType) {
            return containsTypeVariables(((GenericArrayType) type).getGenericComponentType());
        }
        return false;
    }

    private static boolean containsVariableTypeSameParametrizedTypeBound(final TypeVariable<?> typeVariable, final ParameterizedType parameterizedType) {
        return ArrayUtils.contains(typeVariable.getBounds(), parameterizedType);
    }

    /**
     * Tries to determine the type arguments of a class/interface based on a super parameterized type's type arguments. This method is the inverse of
     * {@link #getTypeArguments(Type, Class)} which gets a class/interface's type arguments based on a subtype. It is far more limited in determining the type
     * arguments for the subject class's type variables in that it can only determine those parameters that map from the subject {@link Class} object to the
     * supertype.
     *
     * <p>
     * Example: {@link java.util.TreeSet TreeSet} sets its parameter as the parameter for {@link java.util.NavigableSet NavigableSet}, which in turn sets the
     * parameter of {@link java.util.SortedSet}, which in turn sets the parameter of {@link Set}, which in turn sets the parameter of
     * {@link java.util.Collection}, which in turn sets the parameter of {@link Iterable}. Since {@link TreeSet}'s parameter maps (indirectly) to
     * {@link Iterable}'s parameter, it will be able to determine that based on the super type {@code Iterable<? extends
     * Map<Integer, ? extends Collection<?>>>}, the parameter of {@link TreeSet} is {@code ? extends Map<Integer, ? extends
     * Collection<?>>}.
     * </p>
     *
     * @param cls                    the class whose type parameters are to be determined, not {@code null}
     * @param superParameterizedType the super type from which {@code cls}'s type arguments are to be determined, not {@code null}
     * @return a {@link Map} of the type assignments that could be determined for the type variables in each type in the inheritance hierarchy from {@code type}
     *         to {@code toClass} inclusive.
     * @throws NullPointerException if either {@code cls} or {@code superParameterizedType} is {@code null}
     */
    public static Map<TypeVariable<?>, Type> determineTypeArguments(final Class<?> cls, final ParameterizedType superParameterizedType) {
        Objects.requireNonNull(cls, "cls");
        Objects.requireNonNull(superParameterizedType, "superParameterizedType");
        final Class<?> superClass = getRawType(superParameterizedType);
        // compatibility check
        if (!isAssignable(cls, superClass)) {
            return null;
        }
        if (cls.equals(superClass)) {
            return getTypeArguments(superParameterizedType, superClass, null);
        }
        // get the next class in the inheritance hierarchy
        final Type midType = getClosestParentType(cls, superClass);
        // can only be a class or a parameterized type
        if (midType instanceof Class<?>) {
            return determineTypeArguments((Class<?>) midType, superParameterizedType);
        }
        final ParameterizedType midParameterizedType = (ParameterizedType) midType;
        final Class<?> midClass = getRawType(midParameterizedType);
        // get the type variables of the mid class that map to the type
        // arguments of the super class
        final Map<TypeVariable<?>, Type> typeVarAssigns = determineTypeArguments(midClass, superParameterizedType);
        // map the arguments of the mid type to the class type variables
        mapTypeVariablesToArguments(cls, midParameterizedType, typeVarAssigns);
        return typeVarAssigns;
    }

    /**
     * Tests whether {@code t} equals {@code a}.
     *
     * @param genericArrayType LHS
     * @param type             RHS
     * @return boolean
     */
    private static boolean equals(final GenericArrayType genericArrayType, final Type type) {
        return type instanceof GenericArrayType && equals(genericArrayType.getGenericComponentType(), ((GenericArrayType) type).getGenericComponentType());
    }

    /**
     * Tests whether {@code t} equals {@code p}.
     *
     * @param parameterizedType LHS
     * @param type              RHS
     * @return boolean
     */
    private static boolean equals(final ParameterizedType parameterizedType, final Type type) {
        if (type instanceof ParameterizedType) {
            final ParameterizedType other = (ParameterizedType) type;
            if (equals(parameterizedType.getRawType(), other.getRawType()) && equals(parameterizedType.getOwnerType(), other.getOwnerType())) {
                return equals(parameterizedType.getActualTypeArguments(), other.getActualTypeArguments());
            }
        }
        return false;
    }

    /**
     * Tests equality of types.
     *
     * @param type1 the first type
     * @param type2 the second type
     * @return boolean
     * @since 3.2
     */
    public static boolean equals(final Type type1, final Type type2) {
        if (Objects.equals(type1, type2)) {
            return true;
        }
        if (type1 instanceof ParameterizedType) {
            return equals((ParameterizedType) type1, type2);
        }
        if (type1 instanceof GenericArrayType) {
            return equals((GenericArrayType) type1, type2);
        }
        if (type1 instanceof WildcardType) {
            return equals((WildcardType) type1, type2);
        }
        return false;
    }

    /**
     * Tests whether {@code t1} equals {@code t2}.
     *
     * @param type1 LHS
     * @param type2 RHS
     * @return boolean
     */
    private static boolean equals(final Type[] type1, final Type[] type2) {
        if (type1.length == type2.length) {
            for (int i = 0; i < type1.length; i++) {
                if (!equals(type1[i], type2[i])) {
                    return false;
                }
            }
            return true;
        }
        return false;
    }

    /**
     * Tests whether {@code t} equals {@code w}.
     *
     * @param wildcardType LHS
     * @param type         RHS
     * @return boolean
     */
    private static boolean equals(final WildcardType wildcardType, final Type type) {
        if (type instanceof WildcardType) {
            final WildcardType other = (WildcardType) type;
            return equals(getImplicitLowerBounds(wildcardType), getImplicitLowerBounds(other))
                    && equals(getImplicitUpperBounds(wildcardType), getImplicitUpperBounds(other));
        }
        return false;
    }

    /**
     * Helper method to establish the formal parameters for a parameterized type.
     *
     * @param mappings  map containing the assignments
     * @param variables expected map keys
     * @return array of map values corresponding to specified keys
     */
    private static Type[] extractTypeArgumentsFrom(final Map<TypeVariable<?>, Type> mappings, final TypeVariable<?>[] variables) {
        final Type[] result = new Type[variables.length];
        int index = 0;
        for (final TypeVariable<?> var : variables) {
            Validate.isTrue(mappings.containsKey(var), () -> String.format("missing argument mapping for %s", toString(var)));
            result[index++] = mappings.get(var);
        }
        return result;
    }

    private static int[] findRecursiveTypes(final ParameterizedType parameterizedType) {
        final Type[] filteredArgumentTypes = Arrays.copyOf(parameterizedType.getActualTypeArguments(), parameterizedType.getActualTypeArguments().length);
        int[] indexesToRemove = {};
        for (int i = 0; i < filteredArgumentTypes.length; i++) {
            if (filteredArgumentTypes[i] instanceof TypeVariable<?>
                    && containsVariableTypeSameParametrizedTypeBound((TypeVariable<?>) filteredArgumentTypes[i], parameterizedType)) {
                indexesToRemove = ArrayUtils.add(indexesToRemove, i);
            }
        }
        return indexesToRemove;
    }

    /**
     * Creates a generic array type instance.
     *
     * @param componentType the type of the elements of the array. For example the component type of {@code boolean[]} is {@code boolean}
     * @return {@link GenericArrayType}
     * @since 3.2
     */
    public static GenericArrayType genericArrayType(final Type componentType) {
        return new GenericArrayTypeImpl(Objects.requireNonNull(componentType, "componentType"));
    }

    /**
     * Formats a {@link GenericArrayType} as a {@link String}.
     *
     * @param genericArrayType {@link GenericArrayType} to format
     * @return String
     */
    private static String genericArrayTypeToString(final GenericArrayType genericArrayType) {
        return String.format("%s[]", toString(genericArrayType.getGenericComponentType()));
    }

    /**
     * Gets the array component type of {@code type}.
     *
     * @param type the type to be checked
     * @return component type or null if type is not an array type
     */
    public static Type getArrayComponentType(final Type type) {
        if (type instanceof Class<?>) {
            final Class<?> cls = (Class<?>) type;
            return cls.isArray() ? cls.getComponentType() : null;
        }
        if (type instanceof GenericArrayType) {
            return ((GenericArrayType) type).getGenericComponentType();
        }
        return null;
    }

    /**
     * Gets the closest parent type to the super class specified by {@code superClass}.
     *
     * @param cls        the class in question
     * @param superClass the super class
     * @return the closes parent type
     */
    private static Type getClosestParentType(final Class<?> cls, final Class<?> superClass) {
        // only look at the interfaces if the super class is also an interface
        if (superClass.isInterface()) {
            // get the generic interfaces of the subject class
            final Type[] interfaceTypes = cls.getGenericInterfaces();
            // will hold the best generic interface match found
            Type genericInterface = null;
            // find the interface closest to the super class
            for (final Type midType : interfaceTypes) {
                final Class<?> midClass;
                if (midType instanceof ParameterizedType) {
                    midClass = getRawType((ParameterizedType) midType);
                } else if (midType instanceof Class<?>) {
                    midClass = (Class<?>) midType;
                } else {
                    throw new IllegalStateException("Unexpected generic" + " interface type found: " + midType);
                }
                // check if this interface is further up the inheritance chain
                // than the previously found match
                if (isAssignable(midClass, superClass) && isAssignable(genericInterface, (Type) midClass)) {
                    genericInterface = midType;
                }
            }
            // found a match?
            if (genericInterface != null) {
                return genericInterface;
            }
        }
        // none of the interfaces were descendants of the target class, so the
        // super class has to be one, instead
        return cls.getGenericSuperclass();
    }

    /**
     * Gets an array containing the sole type of {@link Object} if {@link TypeVariable#getBounds()} returns an empty array. Otherwise, it returns the result of
     * {@link TypeVariable#getBounds()} passed into {@link #normalizeUpperBounds}.
     *
     * @param typeVariable the subject type variable, not {@code null}
     * @return a non-empty array containing the bounds of the type variable, which could be {@link Object}.
     * @throws NullPointerException if {@code typeVariable} is {@code null}
     */
    public static Type[] getImplicitBounds(final TypeVariable<?> typeVariable) {
        return normalizeUpperToObject(Objects.requireNonNull(typeVariable, "typeVariable").getBounds());
    }

    /**
     * Gets an array containing a single value of {@code null} if {@link WildcardType#getLowerBounds()} returns an empty array. Otherwise, it returns the result
     * of {@link WildcardType#getLowerBounds()}.
     *
     * @param wildcardType the subject wildcard type, not {@code null}
     * @return a non-empty array containing the lower bounds of the wildcard type, which could be null.
     * @throws NullPointerException if {@code wildcardType} is {@code null}
     */
    public static Type[] getImplicitLowerBounds(final WildcardType wildcardType) {
        Objects.requireNonNull(wildcardType, "wildcardType");
        final Type[] bounds = wildcardType.getLowerBounds();
        return bounds.length == 0 ? new Type[] { null } : bounds;
    }

    /**
     * Gets an array containing the sole value of {@link Object} if {@link WildcardType#getUpperBounds()} returns an empty array. Otherwise, it returns the
     * result of {@link WildcardType#getUpperBounds()} passed into {@link #normalizeUpperBounds}.
     *
     * @param wildcardType the subject wildcard type, not {@code null}
     * @return a non-empty array containing the upper bounds of the wildcard type.
     * @throws NullPointerException if {@code wildcardType} is {@code null}
     */
    public static Type[] getImplicitUpperBounds(final WildcardType wildcardType) {
        return normalizeUpperToObject(Objects.requireNonNull(wildcardType, "wildcardType").getUpperBounds());
    }

    /**
     * Transforms the passed in type to a {@link Class} object. Type-checking method of convenience.
     *
     * @param parameterizedType the type to be converted
     * @return the corresponding {@link Class} object
     * @throws IllegalStateException if the conversion fails
     */
    private static Class<?> getRawType(final ParameterizedType parameterizedType) {
        final Type rawType = parameterizedType.getRawType();
        // check if raw type is a Class object
        // not currently necessary, but since the return type is Type instead of
        // Class, there's enough reason to believe that future versions of Java
        // may return other Type implementations. And type-safety checking is
        // rarely a bad idea.
        if (!(rawType instanceof Class<?>)) {
            throw new IllegalStateException("Type of rawType: " + rawType);
        }
        return (Class<?>) rawType;
    }

    /**
     * Gets the raw type of a Java type, given its context. Primarily for use with {@link TypeVariable}s and {@link GenericArrayType}s, or when you do not know
     * the runtime type of {@code type}: if you know you have a {@link Class} instance, it is already raw; if you know you have a {@link ParameterizedType}, its
     * raw type is only a method call away.
     *
     * @param type          to resolve
     * @param assigningType type to be resolved against
     * @return the resolved {@link Class} object or {@code null} if the type could not be resolved
     */
    public static Class<?> getRawType(final Type type, final Type assigningType) {
        if (type instanceof Class<?>) {
            // it is raw, no problem
            return (Class<?>) type;
        }
        if (type instanceof ParameterizedType) {
            // simple enough to get the raw type of a ParameterizedType
            return getRawType((ParameterizedType) type);
        }
        if (type instanceof TypeVariable<?>) {
            if (assigningType == null) {
                return null;
            }
            // get the entity declaring this type variable
            final Object genericDeclaration = ((TypeVariable<?>) type).getGenericDeclaration();
            // can't get the raw type of a method- or constructor-declared type
            // variable
            if (!(genericDeclaration instanceof Class<?>)) {
                return null;
            }
            // get the type arguments for the declaring class/interface based
            // on the enclosing type
            final Map<TypeVariable<?>, Type> typeVarAssigns = getTypeArguments(assigningType, (Class<?>) genericDeclaration);
            // enclosingType has to be a subclass (or subinterface) of the
            // declaring type
            if (typeVarAssigns == null) {
                return null;
            }
            // get the argument assigned to this type variable
            final Type typeArgument = typeVarAssigns.get(type);
            if (typeArgument == null) {
                return null;
            }
            // get the argument for this type variable
            return getRawType(typeArgument, assigningType);
        }
        if (type instanceof GenericArrayType) {
            // get raw component type
            final Class<?> rawComponentType = getRawType(((GenericArrayType) type).getGenericComponentType(), assigningType);
            // create array type from raw component type and return its class
            return rawComponentType != null ? Array.newInstance(rawComponentType, 0).getClass() : null;
        }
        // (hand-waving) this is not the method you're looking for
        if (type instanceof WildcardType) {
            return null;
        }
        throw new IllegalArgumentException("unknown type: " + type);
    }

    /**
     * Gets a map of the type arguments of a class in the context of {@code toClass}.
     *
     * @param cls               the class in question
     * @param toClass           the context class
     * @param subtypeVarAssigns a map with type variables
     * @return the {@link Map} with type arguments
     */
    private static Map<TypeVariable<?>, Type> getTypeArguments(Class<?> cls, final Class<?> toClass, final Map<TypeVariable<?>, Type> subtypeVarAssigns) {
        // make sure they're assignable
        if (!isAssignable(cls, toClass)) {
            return null;
        }
        // can't work with primitives
        if (cls.isPrimitive()) {
            // both classes are primitives?
            if (toClass.isPrimitive()) {
                // dealing with widening here. No type arguments to be
                // harvested with these two types.
                return new HashMap<>();
            }
            // work with wrapper the wrapper class instead of the primitive
            cls = ClassUtils.primitiveToWrapper(cls);
        }
        // create a copy of the incoming map, or an empty one if it's null
        final HashMap<TypeVariable<?>, Type> typeVarAssigns = subtypeVarAssigns == null ? new HashMap<>() : new HashMap<>(subtypeVarAssigns);
        // has target class been reached?
        if (toClass.equals(cls)) {
            return typeVarAssigns;
        }
        // walk the inheritance hierarchy until the target class is reached
        return getTypeArguments(getClosestParentType(cls, toClass), toClass, typeVarAssigns);
    }

    /**
     * Gets all the type arguments for this parameterized type including owner hierarchy arguments such as {@code Outer<K, V>.Inner<T>.DeepInner<E>} . The
     * arguments are returned in a {@link Map} specifying the argument type for each {@link TypeVariable}.
     *
     * @param type specifies the subject parameterized type from which to harvest the parameters.
     * @return a {@link Map} of the type arguments to their respective type variables.
     */
    public static Map<TypeVariable<?>, Type> getTypeArguments(final ParameterizedType type) {
        return getTypeArguments(type, getRawType(type), null);
    }

    /**
     * Gets a map of the type arguments of a parameterized type in the context of {@code toClass}.
     *
     * @param parameterizedType the parameterized type
     * @param toClass           the class
     * @param subtypeVarAssigns a map with type variables
     * @return the {@link Map} with type arguments
     */
    private static Map<TypeVariable<?>, Type> getTypeArguments(final ParameterizedType parameterizedType, final Class<?> toClass,
            final Map<TypeVariable<?>, Type> subtypeVarAssigns) {
        final Class<?> cls = getRawType(parameterizedType);
        // make sure they're assignable
        if (!isAssignable(cls, toClass)) {
            return null;
        }
        final Type ownerType = parameterizedType.getOwnerType();
        final Map<TypeVariable<?>, Type> typeVarAssigns;
        if (ownerType instanceof ParameterizedType) {
            // get the owner type arguments first
            final ParameterizedType parameterizedOwnerType = (ParameterizedType) ownerType;
            typeVarAssigns = getTypeArguments(parameterizedOwnerType, getRawType(parameterizedOwnerType), subtypeVarAssigns);
        } else {
            // no owner, prep the type variable assignments map
            typeVarAssigns = subtypeVarAssigns == null ? new HashMap<>() : new HashMap<>(subtypeVarAssigns);
        }
        // get the subject parameterized type's arguments
        final Type[] typeArgs = parameterizedType.getActualTypeArguments();
        // and get the corresponding type variables from the raw class
        final TypeVariable<?>[] typeParams = cls.getTypeParameters();
        // map the arguments to their respective type variables
        for (int i = 0; i < typeParams.length; i++) {
            final Type typeArg = typeArgs[i];
            typeVarAssigns.put(typeParams[i], typeVarAssigns.getOrDefault(typeArg, typeArg));
        }
        if (toClass.equals(cls)) {
            // target class has been reached. Done.
            return typeVarAssigns;
        }
        // walk the inheritance hierarchy until the target class is reached
        return getTypeArguments(getClosestParentType(cls, toClass), toClass, typeVarAssigns);
    }

    /**
     * Gets the type arguments of a class/interface based on a subtype. For instance, this method will determine that both of the parameters for the interface
     * {@link Map} are {@link Object} for the subtype {@link java.util.Properties Properties} even though the subtype does not directly implement the
     * {@link Map} interface.
     *
     * <p>
     * This method returns {@code null} if {@code type} is not assignable to {@code toClass}. It returns an empty map if none of the classes or interfaces in
     * its inheritance hierarchy specify any type arguments.
     * </p>
     *
     * <p>
     * A side effect of this method is that it also retrieves the type arguments for the classes and interfaces that are part of the hierarchy between
     * {@code type} and {@code toClass}. So with the above example, this method will also determine that the type arguments for {@link java.util.Hashtable
     * Hashtable} are also both {@link Object}. In cases where the interface specified by {@code toClass} is (indirectly) implemented more than once (e.g. where
     * {@code toClass} specifies the interface {@link Iterable Iterable} and {@code type} specifies a parameterized type that implements both
     * {@link java.util.Set Set} and {@link java.util.Collection Collection}), this method will look at the inheritance hierarchy of only one of the
     * implementations/subclasses; the first interface encountered that isn't a subinterface to one of the others in the {@code type} to {@code toClass}
     * hierarchy.
     * </p>
     *
     * @param type    the type from which to determine the type parameters of {@code toClass}
     * @param toClass the class whose type parameters are to be determined based on the subtype {@code type}
     * @return a {@link Map} of the type assignments for the type variables in each type in the inheritance hierarchy from {@code type} to {@code toClass}
     *         inclusive.
     */
    public static Map<TypeVariable<?>, Type> getTypeArguments(final Type type, final Class<?> toClass) {
        return getTypeArguments(type, toClass, null);
    }

    /**
     * Gets a map of the type arguments of {@code type} in the context of {@code toClass}.
     *
     * @param type              the type in question
     * @param toClass           the class
     * @param subtypeVarAssigns a map with type variables
     * @return the {@link Map} with type arguments
     */
    private static Map<TypeVariable<?>, Type> getTypeArguments(final Type type, final Class<?> toClass, final Map<TypeVariable<?>, Type> subtypeVarAssigns) {
        if (type instanceof Class<?>) {
            return getTypeArguments((Class<?>) type, toClass, subtypeVarAssigns);
        }
        if (type instanceof ParameterizedType) {
            return getTypeArguments((ParameterizedType) type, toClass, subtypeVarAssigns);
        }
        if (type instanceof GenericArrayType) {
            return getTypeArguments(((GenericArrayType) type).getGenericComponentType(), toClass.isArray() ? toClass.getComponentType() : toClass,
                    subtypeVarAssigns);
        }
        // since wildcard types are not assignable to classes, should this just
        // return null?
        if (type instanceof WildcardType) {
            for (final Type bound : getImplicitUpperBounds((WildcardType) type)) {
                // find the first bound that is assignable to the target class
                if (isAssignable(bound, toClass)) {
                    return getTypeArguments(bound, toClass, subtypeVarAssigns);
                }
            }
            return null;
        }
        if (type instanceof TypeVariable<?>) {
            for (final Type bound : getImplicitBounds((TypeVariable<?>) type)) {
                // find the first bound that is assignable to the target class
                if (isAssignable(bound, toClass)) {
                    return getTypeArguments(bound, toClass, subtypeVarAssigns);
                }
            }
            return null;
        }
        throw new IllegalStateException("found an unhandled type: " + type);
    }

    /**
     * Tests whether the specified type denotes an array type.
     *
     * @param type the type to be checked
     * @return {@code true} if {@code type} is an array class or a {@link GenericArrayType}.
     */
    public static boolean isArrayType(final Type type) {
        return type instanceof GenericArrayType || type instanceof Class<?> && ((Class<?>) type).isArray();
    }

    /**
     * Tests if the subject type may be implicitly cast to the target class following the Java generics rules.
     *
     * @param type    the subject type to be assigned to the target type
     * @param toClass the target class
     * @return {@code true} if {@code type} is assignable to {@code toClass}.
     */
    private static boolean isAssignable(final Type type, final Class<?> toClass) {
        if (type == null) {
            // consistency with ClassUtils.isAssignable() behavior
            return toClass == null || !toClass.isPrimitive();
        }
        // only a null type can be assigned to null type which
        // would have cause the previous to return true
        if (toClass == null) {
            return false;
        }
        // all types are assignable to themselves
        if (toClass.equals(type)) {
            return true;
        }
        if (type instanceof Class<?>) {
            // just comparing two classes
            return ClassUtils.isAssignable((Class<?>) type, toClass);
        }
        if (type instanceof ParameterizedType) {
            // only have to compare the raw type to the class
            return isAssignable(getRawType((ParameterizedType) type), toClass);
        }
        // *
        if (type instanceof TypeVariable<?>) {
            // if any of the bounds are assignable to the class, then the
            // type is assignable to the class.
            for (final Type bound : ((TypeVariable<?>) type).getBounds()) {
                if (isAssignable(bound, toClass)) {
                    return true;
                }
            }
            return false;
        }
        // the only classes to which a generic array type can be assigned
        // are class Object and array classes
        if (type instanceof GenericArrayType) {
            return toClass.equals(Object.class)
                    || toClass.isArray() && isAssignable(((GenericArrayType) type).getGenericComponentType(), toClass.getComponentType());
        }
        // wildcard types are not assignable to a class (though one would think
        // "? super Object" would be assignable to Object)
        if (type instanceof WildcardType) {
            return false;
        }
        throw new IllegalStateException("found an unhandled type: " + type);
    }

    /**
     * Tests if the subject type may be implicitly cast to the target generic array type following the Java generics rules.
     *
     * @param type               the subject type to be assigned to the target type
     * @param toGenericArrayType the target generic array type
     * @param typeVarAssigns     a map with type variables
     * @return {@code true} if {@code type} is assignable to {@code toGenericArrayType}.
     */
    private static boolean isAssignable(final Type type, final GenericArrayType toGenericArrayType, final Map<TypeVariable<?>, Type> typeVarAssigns) {
        if (type == null) {
            return true;
        }
        // only a null type can be assigned to null type which
        // would have cause the previous to return true
        if (toGenericArrayType == null) {
            return false;
        }
        // all types are assignable to themselves
        if (toGenericArrayType.equals(type)) {
            return true;
        }
        final Type toComponentType = toGenericArrayType.getGenericComponentType();
        if (type instanceof Class<?>) {
            final Class<?> cls = (Class<?>) type;
            // compare the component types
            return cls.isArray() && isAssignable(cls.getComponentType(), toComponentType, typeVarAssigns);
        }
        if (type instanceof GenericArrayType) {
            // compare the component types
            return isAssignable(((GenericArrayType) type).getGenericComponentType(), toComponentType, typeVarAssigns);
        }
        if (type instanceof WildcardType) {
            // so long as one of the upper bounds is assignable, it's good
            for (final Type bound : getImplicitUpperBounds((WildcardType) type)) {
                if (isAssignable(bound, toGenericArrayType)) {
                    return true;
                }
            }
            return false;
        }
        if (type instanceof TypeVariable<?>) {
            // probably should remove the following logic and just return false.
            // type variables cannot specify arrays as bounds.
            for (final Type bound : getImplicitBounds((TypeVariable<?>) type)) {
                if (isAssignable(bound, toGenericArrayType)) {
                    return true;
                }
            }
            return false;
        }
        if (type instanceof ParameterizedType) {
            // the raw type of a parameterized type is never an array or
            // generic array, otherwise the declaration would look like this:
            // Collection[]< ? extends String > collection;
            return false;
        }
        throw new IllegalStateException("found an unhandled type: " + type);
    }

    /**
     * Tests if the subject type may be implicitly cast to the target parameterized type following the Java generics rules.
     *
     * @param type                the subject type to be assigned to the target type
     * @param toParameterizedType the target parameterized type
     * @param typeVarAssigns      a map with type variables
     * @return {@code true} if {@code type} is assignable to {@code toType}.
     */
    private static boolean isAssignable(final Type type, final ParameterizedType toParameterizedType, final Map<TypeVariable<?>, Type> typeVarAssigns) {
        if (type == null) {
            return true;
        }
        // only a null type can be assigned to null type which
        // would have cause the previous to return true
        if (toParameterizedType == null) {
            return false;
        }
        // cannot cast an array type to a parameterized type.
        if (type instanceof GenericArrayType) {
            return false;
        }
        // all types are assignable to themselves
        if (toParameterizedType.equals(type)) {
            return true;
        }
        // get the target type's raw type
        final Class<?> toClass = getRawType(toParameterizedType);
        // get the subject type's type arguments including owner type arguments
        // and supertype arguments up to and including the target class.
        final Map<TypeVariable<?>, Type> fromTypeVarAssigns = getTypeArguments(type, toClass, null);
        // null means the two types are not compatible
        if (fromTypeVarAssigns == null) {
            return false;
        }
        // compatible types, but there's no type arguments. this is equivalent
        // to comparing Map< ?, ? > to Map, and raw types are always assignable
        // to parameterized types.
        if (fromTypeVarAssigns.isEmpty()) {
            return true;
        }
        // get the target type's type arguments including owner type arguments
        final Map<TypeVariable<?>, Type> toTypeVarAssigns = getTypeArguments(toParameterizedType, toClass, typeVarAssigns);
        // now to check each type argument
        for (final TypeVariable<?> var : toTypeVarAssigns.keySet()) {
            final Type toTypeArg = unrollVariableAssignments(var, toTypeVarAssigns);
            final Type fromTypeArg = unrollVariableAssignments(var, fromTypeVarAssigns);
            if (toTypeArg == null && fromTypeArg instanceof Class) {
                continue;
            }
            // parameters must either be absent from the subject type, within
            // the bounds of the wildcard type, or be an exact match to the
            // parameters of the target type.
            if (fromTypeArg != null && toTypeArg != null && !toTypeArg.equals(fromTypeArg)
                    && !(toTypeArg instanceof WildcardType && isAssignable(fromTypeArg, toTypeArg, typeVarAssigns))) {
                return false;
            }
        }
        return true;
    }

    /**
     * Tests if the subject type may be implicitly cast to the target type following the Java generics rules. If both types are {@link Class} objects, the
     * method returns the result of {@link ClassUtils#isAssignable(Class, Class)}.
     *
     * @param type   the subject type to be assigned to the target type
     * @param toType the target type
     * @return {@code true} if {@code type} is assignable to {@code toType}.
     */
    public static boolean isAssignable(final Type type, final Type toType) {
        return isAssignable(type, toType, null);
    }

    /**
     * Tests if the subject type may be implicitly cast to the target type following the Java generics rules.
     *
     * @param type           the subject type to be assigned to the target type
     * @param toType         the target type
     * @param typeVarAssigns optional map of type variable assignments
     * @return {@code true} if {@code type} is assignable to {@code toType}.
     */
    private static boolean isAssignable(final Type type, final Type toType, final Map<TypeVariable<?>, Type> typeVarAssigns) {
        if (toType == null || toType instanceof Class<?>) {
            return isAssignable(type, (Class<?>) toType);
        }
        if (toType instanceof ParameterizedType) {
            return isAssignable(type, (ParameterizedType) toType, typeVarAssigns);
        }
        if (toType instanceof GenericArrayType) {
            return isAssignable(type, (GenericArrayType) toType, typeVarAssigns);
        }
        if (toType instanceof WildcardType) {
            return isAssignable(type, (WildcardType) toType, typeVarAssigns);
        }
        if (toType instanceof TypeVariable<?>) {
            return isAssignable(type, (TypeVariable<?>) toType, typeVarAssigns);
        }
        throw new IllegalStateException("found an unhandled type: " + toType);
    }

    /**
     * Tests if the subject type may be implicitly cast to the target type variable following the Java generics rules.
     *
     * @param type           the subject type to be assigned to the target type
     * @param toTypeVariable the target type variable
     * @param typeVarAssigns a map with type variables
     * @return {@code true} if {@code type} is assignable to {@code toTypeVariable}.
     */
    private static boolean isAssignable(final Type type, final TypeVariable<?> toTypeVariable, final Map<TypeVariable<?>, Type> typeVarAssigns) {
        if (type == null) {
            return true;
        }
        // only a null type can be assigned to null type which
        // would have cause the previous to return true
        if (toTypeVariable == null) {
            return false;
        }
        // all types are assignable to themselves
        if (toTypeVariable.equals(type)) {
            return true;
        }
        if (type instanceof TypeVariable<?>) {
            // a type variable is assignable to another type variable, if
            // and only if the former is the latter, extends the latter, or
            // is otherwise a descendant of the latter.
            final Type[] bounds = getImplicitBounds((TypeVariable<?>) type);
            for (final Type bound : bounds) {
                if (isAssignable(bound, toTypeVariable, typeVarAssigns)) {
                    return true;
                }
            }
        }
        if (type instanceof Class<?> || type instanceof ParameterizedType || type instanceof GenericArrayType || type instanceof WildcardType) {
            return false;
        }
        throw new IllegalStateException("found an unhandled type: " + type);
    }

    /**
     * Tests if the subject type may be implicitly cast to the target wildcard type following the Java generics rules.
     *
     * @param type           the subject type to be assigned to the target type
     * @param toWildcardType the target wildcard type
     * @param typeVarAssigns a map with type variables
     * @return {@code true} if {@code type} is assignable to {@code toWildcardType}.
     */
    private static boolean isAssignable(final Type type, final WildcardType toWildcardType, final Map<TypeVariable<?>, Type> typeVarAssigns) {
        if (type == null) {
            return true;
        }
        // only a null type can be assigned to null type which
        // would have cause the previous to return true
        if (toWildcardType == null) {
            return false;
        }
        // all types are assignable to themselves
        if (toWildcardType.equals(type)) {
            return true;
        }
        final Type[] toUpperBounds = getImplicitUpperBounds(toWildcardType);
        final Type[] toLowerBounds = getImplicitLowerBounds(toWildcardType);
        if (type instanceof WildcardType) {
            final WildcardType wildcardType = (WildcardType) type;
            final Type[] upperBounds = getImplicitUpperBounds(wildcardType);
            final Type[] lowerBounds = getImplicitLowerBounds(wildcardType);
            for (Type toBound : toUpperBounds) {
                // if there are assignments for unresolved type variables,
                // now's the time to substitute them.
                toBound = substituteTypeVariables(toBound, typeVarAssigns);
                // each upper bound of the subject type has to be assignable to
                // each
                // upper bound of the target type
                for (final Type bound : upperBounds) {
                    if (!isAssignable(bound, toBound, typeVarAssigns)) {
                        return false;
                    }
                }
            }
            for (Type toBound : toLowerBounds) {
                // if there are assignments for unresolved type variables,
                // now's the time to substitute them.
                toBound = substituteTypeVariables(toBound, typeVarAssigns);
                // each lower bound of the target type has to be assignable to
                // each
                // lower bound of the subject type
                for (final Type bound : lowerBounds) {
                    if (!isAssignable(toBound, bound, typeVarAssigns)) {
                        return false;
                    }
                }
            }
            return true;
        }
        for (final Type toBound : toUpperBounds) {
            // if there are assignments for unresolved type variables,
            // now's the time to substitute them.
            if (!isAssignable(type, substituteTypeVariables(toBound, typeVarAssigns), typeVarAssigns)) {
                return false;
            }
        }
        for (final Type toBound : toLowerBounds) {
            // if there are assignments for unresolved type variables,
            // now's the time to substitute them.
            if (!isAssignable(substituteTypeVariables(toBound, typeVarAssigns), type, typeVarAssigns)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Tests whether the class contains a cyclical reference in the qualified name of a class. If any of the type parameters of A class is extending X class
     * which is in scope of A class, then it forms cycle.
     *
     * @param cls the class to test.
     * @return whether the class contains a cyclical reference.
     */
    private static boolean isCyclical(final Class<?> cls) {
        for (final TypeVariable<?> typeParameter : cls.getTypeParameters()) {
            for (final Type bound : typeParameter.getBounds()) {
                if (bound.getTypeName().contains(cls.getName())) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Tests if the given value can be assigned to the target type following the Java generics rules.
     *
     * @param value the value to be checked
     * @param type  the target type
     * @return {@code true} if {@code value} is an instance of {@code type}.
     */
    public static boolean isInstance(final Object value, final Type type) {
        if (type == null) {
            return false;
        }
        return value == null ? !(type instanceof Class<?>) || !((Class<?>) type).isPrimitive() : isAssignable(value.getClass(), type, null);
    }

    /**
     * Maps type variables.
     *
     * @param <T>               the generic type of the class in question
     * @param cls               the class in question
     * @param parameterizedType the parameterized type
     * @param typeVarAssigns    the map to be filled
     */
    private static <T> void mapTypeVariablesToArguments(final Class<T> cls, final ParameterizedType parameterizedType,
            final Map<TypeVariable<?>, Type> typeVarAssigns) {
        // capture the type variables from the owner type that have assignments
        final Type ownerType = parameterizedType.getOwnerType();
        if (ownerType instanceof ParameterizedType) {
            // recursion to make sure the owner's owner type gets processed
            mapTypeVariablesToArguments(cls, (ParameterizedType) ownerType, typeVarAssigns);
        }
        // parameterizedType is a generic interface/class (or it's in the owner
        // hierarchy of said interface/class) implemented/extended by the class
        // cls. Find out which type variables of cls are type arguments of
        // parameterizedType:
        final Type[] typeArgs = parameterizedType.getActualTypeArguments();
        // of the cls's type variables that are arguments of parameterizedType,
        // find out which ones can be determined from the super type's arguments
        final TypeVariable<?>[] typeVars = getRawType(parameterizedType).getTypeParameters();
        // use List view of type parameters of cls so the contains() method can be used:
        final List<TypeVariable<Class<T>>> typeVarList = Arrays.asList(cls.getTypeParameters());
        for (int i = 0; i < typeArgs.length; i++) {
            final TypeVariable<?> typeVar = typeVars[i];
            final Type typeArg = typeArgs[i];
            // argument of parameterizedType is a type variable of cls
            if (typeVarList.contains(typeArg)
                    // type variable of parameterizedType has an assignment in
                    // the super type.
                    && typeVarAssigns.containsKey(typeVar)) {
                // map the assignment to the cls's type variable
                typeVarAssigns.put((TypeVariable<?>) typeArg, typeVarAssigns.get(typeVar));
            }
        }
    }

    /**
     * Strips out the redundant upper bound types in type variable types and wildcard types (or it would with wildcard types if multiple upper bounds were
     * allowed).
     *
     * <p>
     * Example, with the variable type declaration:
     * </p>
     *
     * <pre>{@code
     * <K extends java.util.Collection<String> & java.util.List<String>>
     * }</pre>
     *
     * <p>
     * since {@link List} is a subinterface of {@link Collection}, this method will return the bounds as if the declaration had been:
     * </p>
     *
     * <pre>{@code
     * <K extends java.util.List<String>>
     * }</pre>
     *
     * @param bounds an array of types representing the upper bounds of either {@link WildcardType} or {@link TypeVariable}, not {@code null}.
     * @return an array containing the values from {@code bounds} minus the redundant types.
     * @throws NullPointerException if {@code bounds} is {@code null}
     */
    public static Type[] normalizeUpperBounds(final Type[] bounds) {
        Objects.requireNonNull(bounds, "bounds");
        // don't bother if there's only one (or none) type
        if (bounds.length < 2) {
            return bounds;
        }
        final Set<Type> types = new HashSet<>(bounds.length);
        for (final Type type1 : bounds) {
            boolean subtypeFound = false;
            for (final Type type2 : bounds) {
                if (type1 != type2 && isAssignable(type2, type1, null)) {
                    subtypeFound = true;
                    break;
                }
            }
            if (!subtypeFound) {
                types.add(type1);
            }
        }
        return types.toArray(ArrayUtils.EMPTY_TYPE_ARRAY);
    }

    /**
     * Delegates to {@link #normalizeUpperBounds(Type[])} unless {@code bounds} is empty in which case return an array with the element {@code Object.class}.
     *
     * @param bounds bounds an array of types representing the upper bounds of either {@link WildcardType} or {@link TypeVariable}, not {@code null}.
     * @return result from {@link #normalizeUpperBounds(Type[])} unless {@code bounds} is empty in which case return an array with the element
     *         {@code Object.class}.
     */
    private static Type[] normalizeUpperToObject(final Type[] bounds) {
        return bounds.length == 0 ? new Type[] { Object.class } : normalizeUpperBounds(bounds);
    }

    /**
     * Creates a parameterized type instance.
     *
     * @param rawClass        the raw class to create a parameterized type instance for
     * @param typeVariableMap the map used for parameterization
     * @return {@link ParameterizedType}
     * @throws NullPointerException if either {@code rawClass} or {@code typeVariableMap} is {@code null}
     * @since 3.2
     */
    public static final ParameterizedType parameterize(final Class<?> rawClass, final Map<TypeVariable<?>, Type> typeVariableMap) {
        Objects.requireNonNull(rawClass, "rawClass");
        Objects.requireNonNull(typeVariableMap, "typeVariableMap");
        return parameterizeWithOwner(null, rawClass, extractTypeArgumentsFrom(typeVariableMap, rawClass.getTypeParameters()));
    }

    /**
     * Creates a parameterized type instance.
     *
     * @param rawClass      the raw class to create a parameterized type instance for
     * @param typeArguments the types used for parameterization
     * @return {@link ParameterizedType}
     * @throws NullPointerException if {@code rawClass} is {@code null}
     * @since 3.2
     */
    public static final ParameterizedType parameterize(final Class<?> rawClass, final Type... typeArguments) {
        return parameterizeWithOwner(null, rawClass, typeArguments);
    }

    /**
     * Formats a {@link ParameterizedType} as a {@link String}.
     *
     * @param parameterizedType {@link ParameterizedType} to format
     * @return String
     */
    private static String parameterizedTypeToString(final ParameterizedType parameterizedType) {
        final StringBuilder builder = new StringBuilder();
        final Type useOwner = parameterizedType.getOwnerType();
        final Class<?> raw = (Class<?>) parameterizedType.getRawType();
        if (useOwner == null) {
            builder.append(raw.getName());
        } else {
            if (useOwner instanceof Class<?>) {
                builder.append(((Class<?>) useOwner).getName());
            } else {
                builder.append(useOwner);
            }
            builder.append('.').append(raw.getSimpleName());
        }
        final int[] recursiveTypeIndexes = findRecursiveTypes(parameterizedType);
        if (recursiveTypeIndexes.length > 0) {
            appendRecursiveTypes(builder, recursiveTypeIndexes, parameterizedType.getActualTypeArguments());
        } else {
            GT_JOINER.join(builder, (Object[]) parameterizedType.getActualTypeArguments());
        }
        return builder.toString();
    }

    /**
     * Creates a parameterized type instance.
     *
     * @param owner           the owning type
     * @param rawClass        the raw class to create a parameterized type instance for
     * @param typeVariableMap the map used for parameterization
     * @return {@link ParameterizedType}
     * @throws NullPointerException if either {@code rawClass} or {@code typeVariableMap} is {@code null}
     * @since 3.2
     */
    public static final ParameterizedType parameterizeWithOwner(final Type owner, final Class<?> rawClass, final Map<TypeVariable<?>, Type> typeVariableMap) {
        Objects.requireNonNull(rawClass, "rawClass");
        Objects.requireNonNull(typeVariableMap, "typeVariableMap");
        return parameterizeWithOwner(owner, rawClass, extractTypeArgumentsFrom(typeVariableMap, rawClass.getTypeParameters()));
    }

    /**
     * Creates a parameterized type instance.
     *
     * @param owner         the owning type
     * @param rawClass      the raw class to create a parameterized type instance for
     * @param typeArguments the types used for parameterization
     * @return {@link ParameterizedType}
     * @throws NullPointerException if {@code rawClass} is {@code null}
     * @since 3.2
     */
    public static final ParameterizedType parameterizeWithOwner(final Type owner, final Class<?> rawClass, final Type... typeArguments) {
        Objects.requireNonNull(rawClass, "rawClass");
        final Type useOwner;
        if (rawClass.getEnclosingClass() == null) {
            Validate.isTrue(owner == null, "no owner allowed for top-level %s", rawClass);
            useOwner = null;
        } else if (owner == null) {
            useOwner = rawClass.getEnclosingClass();
        } else {
            Validate.isTrue(isAssignable(owner, rawClass.getEnclosingClass()), "%s is invalid owner type for parameterized %s", owner, rawClass);
            useOwner = owner;
        }
        Validate.noNullElements(typeArguments, "null type argument at index %s");
        Validate.isTrue(rawClass.getTypeParameters().length == typeArguments.length, "invalid number of type parameters specified: expected %d, got %d",
                rawClass.getTypeParameters().length, typeArguments.length);
        return new ParameterizedTypeImpl(rawClass, useOwner, typeArguments);
    }

    /**
     * Finds the mapping for {@code type} in {@code typeVarAssigns}.
     *
     * @param type           the type to be replaced
     * @param typeVarAssigns the map with type variables
     * @return the replaced type
     * @throws IllegalArgumentException if the type cannot be substituted
     */
    private static Type substituteTypeVariables(final Type type, final Map<TypeVariable<?>, Type> typeVarAssigns) {
        if (type instanceof TypeVariable<?> && typeVarAssigns != null) {
            final Type replacementType = typeVarAssigns.get(type);
            if (replacementType == null) {
                throw new IllegalArgumentException("missing assignment type for type variable " + type);
            }
            return replacementType;
        }
        return type;
    }

    /**
     * Formats a {@link TypeVariable} including its {@link GenericDeclaration}.
     *
     * @param typeVariable the type variable to create a String representation for, not {@code null}
     * @return String
     * @throws NullPointerException if {@code typeVariable} is {@code null}
     * @since 3.2
     */
    public static String toLongString(final TypeVariable<?> typeVariable) {
        Objects.requireNonNull(typeVariable, "typeVariable");
        final StringBuilder buf = new StringBuilder();
        final GenericDeclaration d = typeVariable.getGenericDeclaration();
        if (d instanceof Class<?>) {
            Class<?> c = (Class<?>) d;
            while (true) {
                if (c.getEnclosingClass() == null) {
                    buf.insert(0, c.getName());
                    break;
                }
                buf.insert(0, c.getSimpleName()).insert(0, '.');
                c = c.getEnclosingClass();
            }
        } else if (d instanceof Type) { // not possible as of now
            buf.append(toString((Type) d));
        } else {
            buf.append(d);
        }
        return buf.append(':').append(typeVariableToString(typeVariable)).toString();
    }

    /**
     * Formats a given type as a Java-esque String.
     *
     * @param type the type to create a String representation for, not {@code null}
     * @return String
     * @throws NullPointerException if {@code type} is {@code null}
     * @since 3.2
     */
    public static String toString(final Type type) {
        Objects.requireNonNull(type, "type");
        if (type instanceof Class<?>) {
            return classToString((Class<?>) type);
        }
        if (type instanceof ParameterizedType) {
            return parameterizedTypeToString((ParameterizedType) type);
        }
        if (type instanceof WildcardType) {
            return wildcardTypeToString((WildcardType) type);
        }
        if (type instanceof TypeVariable<?>) {
            return typeVariableToString((TypeVariable<?>) type);
        }
        if (type instanceof GenericArrayType) {
            return genericArrayTypeToString((GenericArrayType) type);
        }
        throw new IllegalArgumentException(ObjectUtils.identityToString(type));
    }

    /**
     * Determines whether or not specified types satisfy the bounds of their mapped type variables. When a type parameter extends another (such as
     * {@code <T, S extends T>}), uses another as a type parameter (such as {@code <T, S extends Comparable>>}), or otherwise depends on another type variable
     * to be specified, the dependencies must be included in {@code typeVarAssigns}.
     *
     * @param typeVariableMap specifies the potential types to be assigned to the type variables, not {@code null}.
     * @return whether or not the types can be assigned to their respective type variables.
     * @throws NullPointerException if {@code typeVariableMap} is {@code null}
     */
    public static boolean typesSatisfyVariables(final Map<TypeVariable<?>, Type> typeVariableMap) {
        Objects.requireNonNull(typeVariableMap, "typeVariableMap");
        // all types must be assignable to all the bounds of their mapped
        // type variable.
        for (final Map.Entry<TypeVariable<?>, Type> entry : typeVariableMap.entrySet()) {
            final TypeVariable<?> typeVar = entry.getKey();
            final Type type = entry.getValue();

            for (final Type bound : getImplicitBounds(typeVar)) {
                if (!isAssignable(type, substituteTypeVariables(bound, typeVariableMap), typeVariableMap)) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Formats a {@link TypeVariable} as a {@link String}.
     *
     * @param typeVariable {@link TypeVariable} to format
     * @return String
     */
    private static String typeVariableToString(final TypeVariable<?> typeVariable) {
        final StringBuilder builder = new StringBuilder(typeVariable.getName());
        final Type[] bounds = typeVariable.getBounds();
        if (bounds.length > 0 && !(bounds.length == 1 && Object.class.equals(bounds[0]))) {
            // https://issues.apache.org/jira/projects/LANG/issues/LANG-1698
            // There must be a better way to avoid a stack overflow on Java 17 and up.
            // Bounds are different in Java 17 and up where instead of Object you can get an interface like Comparable.
            final Type bound = bounds[0];
            boolean append = true;
            if (bound instanceof ParameterizedType) {
                final Type rawType = ((ParameterizedType) bound).getRawType();
                if (rawType instanceof Class && ((Class<?>) rawType).isInterface()) {
                    // Avoid recursion and stack overflow on Java 17 and up.
                    append = false;
                }
            }
            if (append) {
                builder.append(" extends ");
                AMP_JOINER.join(builder, bounds);
            }
        }
        return builder.toString();
    }

    /**
     * Unrolls variables in a type bounds array.
     *
     * @param typeArguments assignments {@link Map}
     * @param bounds        in which to expand variables
     * @return {@code bounds} with any variables reassigned
     */
    private static Type[] unrollBounds(final Map<TypeVariable<?>, Type> typeArguments, final Type[] bounds) {
        Type[] result = bounds;
        int i = 0;
        for (; i < result.length; i++) {
            final Type unrolled = unrollVariables(typeArguments, result[i]);
            if (unrolled == null) {
                result = ArrayUtils.remove(result, i--);
            } else {
                result[i] = unrolled;
            }
        }
        return result;
    }

    /**
     * Looks up {@code typeVariable} in {@code typeVarAssigns} <em>transitively</em>, i.e. keep looking until the value found is <em>not</em> a type variable.
     *
     * @param typeVariable   the type variable to look up
     * @param typeVarAssigns the map used for the look-up
     * @return Type or {@code null} if some variable was not in the map
     */
    private static Type unrollVariableAssignments(TypeVariable<?> typeVariable, final Map<TypeVariable<?>, Type> typeVarAssigns) {
        Type result;
        do {
            result = typeVarAssigns.get(typeVariable);
            if (!(result instanceof TypeVariable<?>) || result.equals(typeVariable)) {
                break;
            }
            typeVariable = (TypeVariable<?>) result;
        } while (true);
        return result;
    }

    /**
     * Gets a type representing {@code type} with variable assignments "unrolled."
     *
     * @param typeArguments as from {@link TypeUtils#getTypeArguments(Type, Class)}
     * @param type          the type to unroll variable assignments for
     * @return Type
     * @since 3.2
     */
    public static Type unrollVariables(Map<TypeVariable<?>, Type> typeArguments, final Type type) {
        if (typeArguments == null) {
            typeArguments = Collections.emptyMap();
        }
        if (containsTypeVariables(type)) {
            if (type instanceof TypeVariable<?>) {
                return unrollVariables(typeArguments, typeArguments.get(type));
            }
            if (type instanceof ParameterizedType) {
                final ParameterizedType p = (ParameterizedType) type;
                final Map<TypeVariable<?>, Type> parameterizedTypeArguments;
                if (p.getOwnerType() == null) {
                    parameterizedTypeArguments = typeArguments;
                } else {
                    parameterizedTypeArguments = new HashMap<>(typeArguments);
                    parameterizedTypeArguments.putAll(getTypeArguments(p));
                }
                final Type[] args = p.getActualTypeArguments();
                for (int i = 0; i < args.length; i++) {
                    final Type unrolled = unrollVariables(parameterizedTypeArguments, args[i]);
                    if (unrolled != null) {
                        args[i] = unrolled;
                    }
                }
                return parameterizeWithOwner(p.getOwnerType(), (Class<?>) p.getRawType(), args);
            }
            if (type instanceof WildcardType) {
                final WildcardType wild = (WildcardType) type;
                return wildcardType().withUpperBounds(unrollBounds(typeArguments, wild.getUpperBounds()))
                        .withLowerBounds(unrollBounds(typeArguments, wild.getLowerBounds())).build();
            }
        }
        return type;
    }

    /**
     * Gets a {@link WildcardTypeBuilder}.
     *
     * @return {@link WildcardTypeBuilder}
     * @since 3.2
     */
    public static WildcardTypeBuilder wildcardType() {
        return new WildcardTypeBuilder();
    }

    /**
     * Formats a {@link WildcardType} as a {@link String}.
     *
     * @param wildcardType {@link WildcardType} to format
     * @return String
     */
    private static String wildcardTypeToString(final WildcardType wildcardType) {
        final StringBuilder builder = new StringBuilder().append('?');
        final Type[] lowerBounds = wildcardType.getLowerBounds();
        final Type[] upperBounds = wildcardType.getUpperBounds();
        if (lowerBounds.length > 1 || lowerBounds.length == 1 && lowerBounds[0] != null) {
            AMP_JOINER.join(builder.append(" super "), lowerBounds);
        } else if (upperBounds.length > 1 || upperBounds.length == 1 && !Object.class.equals(upperBounds[0])) {
            AMP_JOINER.join(builder.append(" extends "), upperBounds);
        }
        return builder.toString();
    }

    /**
     * Wraps the specified {@link Class} in a {@link Typed} wrapper.
     *
     * @param <T>  generic type
     * @param type to wrap
     * @return {@code Typed<T>}
     * @since 3.2
     */
    public static <T> Typed<T> wrap(final Class<T> type) {
        return wrap((Type) type);
    }

    /**
     * Wraps the specified {@link Type} in a {@link Typed} wrapper.
     *
     * @param <T>  inferred generic type
     * @param type to wrap
     * @return {@code Typed<T>}
     * @since 3.2
     */
    public static <T> Typed<T> wrap(final Type type) {
        return () -> type;
    }

    /**
     * {@link TypeUtils} instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code TypeUtils.isAssignable(cls, toClass)}.
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public TypeUtils() {
        // empty
    }

}
