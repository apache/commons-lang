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

import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.lang.reflect.Executable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.ClassUtils;
import org.apache.commons.lang3.ClassUtils.Interfaces;
import org.apache.commons.lang3.Validate;
import org.apache.commons.lang3.stream.LangCollectors;
import org.apache.commons.lang3.stream.Streams;

/**
 * Utility reflection methods focused on {@link Method}s, originally from Commons BeanUtils.
 * Differences from the BeanUtils version may be noted, especially where similar functionality
 * already existed within Lang.
 *
 * <h2>Variable Arity Arguments</h2>
 * <p>
 * The non-exact invocation methods support variable arity methods. Argument types are inferred from argument values, unless explicitly supplied through
 * {@code parameterTypes}. If the argument count matches the declared parameter count and these types are assignment-compatible with the declared parameter
 * types, a null or assignment-compatible final value is passed as the variable arity array without copying. Otherwise, trailing arguments are collected into a
 * new array of the declared component type. This also supports supplying the declared array signature with individual argument values. Reference elements,
 * including {@code null}, are preserved; primitive elements support unboxing and widening conversions.
 * </p>
 *
 * <p>
 * For example, for a target method {@code accept(Object... values)}, {@code invokeMethod(target, "accept", (Object) strings)} passes a {@code String[]} named
 * {@code strings} as the variable arity array. To pass that array as one element instead, use
 * {@code invokeMethod(target, "accept", new Object[] { strings }, new Class<?>[] { Object.class })}.
 * When the argument count matches the declared parameter count, a null final argument with an inferred type or a type assignable to the declared array type
 * represents a null variable arity array. A type assignable only to the reference component type makes it one element, even if that type is itself an array.
 * </p>
 *
 * <p>
 * A passed-through array retains its identity and runtime component type. The invoked method can modify the caller's array, and an incompatible store can throw
 * {@link ArrayStoreException}, wrapped in {@link InvocationTargetException}. Explicit argument types control packing in the same way as expression types in
 * <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.12.4.2">JLS 15.12.4.2</a>; inferred types use runtime information instead.
 * </p>
 *
 * <h2>Known Limitations</h2>
 * <h3>Accessing Public Methods In A Default Access Superclass</h3>
 * <p>
 * There is an issue when invoking {@code public} methods contained in a default access superclass on JREs prior to 1.4.
 * Reflection locates these methods fine and correctly assigns them as {@code public}.
 * However, an {@link IllegalAccessException} is thrown if the method is invoked.
 * </p>
 *
 * <p>
 * {@link MethodUtils} contains a workaround for this situation.
 * It will attempt to call {@link java.lang.reflect.AccessibleObject#setAccessible(boolean)} on this method.
 * If this call succeeds, then the method can be invoked as normal.
 * This call will only succeed when the application has sufficient security privileges.
 * If this call fails then the method may fail.
 * </p>
 *
 * @since 2.5
 */
public class MethodUtils {

    private static final Comparator<Method> METHOD_BY_SIGNATURE = Comparator.comparing(Method::toString);

    /**
     * Computes the aggregate number of inheritance hops between assignable argument class types.  Returns -1
     * if the arguments aren't assignable.  Fills a specific purpose for getMatchingMethod and is not generalized.
     *
     * @param fromClassArray The Class array to calculate the distance from.
     * @param toClassArray The Class array to calculate the distance to.
     * @return The aggregate number of inheritance hops between assignable argument class types.
     */
    private static int distance(final Class<?>[] fromClassArray, final Class<?>[] toClassArray) {
        int answer = 0;
        if (!ClassUtils.isAssignable(fromClassArray, toClassArray, true)) {
            return -1;
        }
        for (int offset = 0; offset < fromClassArray.length; offset++) {
            // Note InheritanceUtils.distance() uses different scoring system.
            final Class<?> aClass = fromClassArray[offset];
            final Class<?> toClass = toClassArray[offset];
            if (aClass == null || aClass.equals(toClass)) {
                continue;
            }
            if (ClassUtils.isAssignable(aClass, toClass, true) && !ClassUtils.isAssignable(aClass, toClass, false)) {
                // Autoboxing/unboxing conversion. When a primitive is boxed, rank the exact
                // wrapper ahead of any of its supertypes so the most specific overload wins.
                answer++;
                if (aClass.isPrimitive() && !ClassUtils.primitiveToWrapper(aClass).equals(toClass)) {
                    answer += 2;
                }
            } else {
                answer += 2;
            }
        }
        return answer;
    }

    /**
     * Gets an accessible method (that is, one that can be invoked via reflection) that implements the specified Method. If no such method can be found, return
     * {@code null}.
     *
     * @param cls The implementing class, may be null.
     * @param method The method that we wish to call, may be null.
     * @return The accessible method or null.
     * @since 3.19.0
     */
    public static Method getAccessibleMethod(final Class<?> cls, final Method method) {
        if (!MemberUtils.isPublic(method)) {
            return null;
        }
        // If the declaring class is public, we are done
        if (ClassUtils.isPublic(cls)) {
            return method;
        }
        final String methodName = method.getName();
        final Class<?>[] parameterTypes = method.getParameterTypes();
        // Check the implemented interfaces and subinterfaces
        final Method method2 = getAccessibleMethodFromInterfaceNest(cls, methodName, parameterTypes);
        // Check the superclass chain
        return method2 != null ? method2 : getAccessibleMethodFromSuperclass(cls, methodName, parameterTypes);
    }

    /**
     * Gets an accessible method (that is, one that can be invoked via reflection) with given name and parameters. If no such method can be found, return
     * {@code null}. This is just a convenience wrapper for {@link #getAccessibleMethod(Method)}.
     *
     * @param cls            get method from this class.
     * @param methodName     get method with this name.
     * @param parameterTypes with these parameters types.
     * @return The accessible method.
     */
    public static Method getAccessibleMethod(final Class<?> cls, final String methodName, final Class<?>... parameterTypes) {
        return getAccessibleMethod(getMethodObject(cls, methodName, parameterTypes));
    }

    /**
     * Gets an accessible method (that is, one that can be invoked via reflection) that implements the specified Method. If no such method can be found, return
     * {@code null}.
     *
     * @param method The method that we wish to call, may be null.
     * @return The accessible method
     */
    public static Method getAccessibleMethod(final Method method) {
        return method != null ? getAccessibleMethod(method.getDeclaringClass(), method) : null;
    }

    /**
     * Gets an accessible method (that is, one that can be invoked via
     * reflection) that implements the specified method, by scanning through
     * all implemented interfaces and subinterfaces. If no such method
     * can be found, return {@code null}.
     *
     * <p>
     * There isn't any good reason why this method must be {@code private}.
     * It is because there doesn't seem any reason why other classes should
     * call this rather than the higher level methods.
     * </p>
     *
     * @param cls Parent class for the interfaces to be checked.
     * @param methodName Method name of the method we wish to call.
     * @param parameterTypes The parameter type signatures.
     * @return The accessible method or {@code null} if not found.
     */
    private static Method getAccessibleMethodFromInterfaceNest(Class<?> cls, final String methodName, final Class<?>... parameterTypes) {
        // Search up the superclass chain
        for (; cls != null; cls = cls.getSuperclass()) {
            // Check the implemented interfaces of the parent class
            final Class<?>[] interfaces = cls.getInterfaces();
            for (final Class<?> anInterface : interfaces) {
                // Is this interface public?
                if (!ClassUtils.isPublic(anInterface)) {
                    continue;
                }
                // Does the method exist on this interface? A static or private one is not inherited.
                try {
                    final Method declared = anInterface.getDeclaredMethod(methodName, parameterTypes);
                    if (MemberUtils.isPublic(declared) && !MemberUtils.isStatic(declared)) {
                        return declared;
                    }
                } catch (final NoSuchMethodException ignored) {
                    /*
                     * Swallow, if no method is found after the loop then this method returns null.
                     */
                }
                // Recursively check our parent interfaces
                final Method method = getAccessibleMethodFromInterfaceNest(anInterface, methodName, parameterTypes);
                if (method != null) {
                    return method;
                }
            }
        }
        return null;
    }

    /**
     * Gets an accessible method (that is, one that can be invoked via
     * reflection) by scanning through the superclasses. If no such method
     * can be found, return {@code null}.
     *
     * @param cls Class to be checked.
     * @param methodName Method name of the method we wish to call.
     * @param parameterTypes The parameter type signatures.
     * @return The accessible method or {@code null} if not found.
     */
    private static Method getAccessibleMethodFromSuperclass(final Class<?> cls, final String methodName, final Class<?>... parameterTypes) {
        Class<?> parentClass = cls.getSuperclass();
        while (parentClass != null) {
            if (ClassUtils.isPublic(parentClass)) {
                return getMethodObject(parentClass, methodName, parameterTypes);
            }
            parentClass = parentClass.getSuperclass();
        }
        return null;
    }

    /**
     * Gets a combination of {@link ClassUtils#getAllSuperclasses(Class)} and {@link ClassUtils#getAllInterfaces(Class)}, one from superclasses, one from
     * interfaces, and so on in a breadth first way.
     *
     * @param cls The class to look up, may be {@code null}.
     * @return The combined {@link List} of superclasses and interfaces in order going up from this one {@code null} if null input.
     */
    private static List<Class<?>> getAllSuperclassesAndInterfaces(final Class<?> cls) {
        if (cls == null) {
            return null;
        }
        final List<Class<?>> allSuperClassesAndInterfaces = new ArrayList<>();
        final List<Class<?>> allSuperclasses = ClassUtils.getAllSuperclasses(cls);
        int superClassIndex = 0;
        final List<Class<?>> allInterfaces = ClassUtils.getAllInterfaces(cls);
        int interfaceIndex = 0;
        while (interfaceIndex < allInterfaces.size() || superClassIndex < allSuperclasses.size()) {
            final Class<?> acls;
            if (interfaceIndex >= allInterfaces.size() || superClassIndex < allSuperclasses.size() && superClassIndex < interfaceIndex) {
                acls = allSuperclasses.get(superClassIndex++);
            } else {
                acls = allInterfaces.get(interfaceIndex++);
            }
            allSuperClassesAndInterfaces.add(acls);
        }
        return allSuperClassesAndInterfaces;
    }

    /**
     * Gets the annotation object with the given annotation type that is present on the given method or optionally on any equivalent method in super classes and
     * interfaces. Returns null if the annotation type was not present.
     *
     * <p>
     * Stops searching for an annotation once the first annotation of the specified type has been found. Additional annotations of the specified type will be
     * silently ignored.
     * </p>
     *
     * @param <A>           the annotation type.
     * @param method        The {@link Method} to query, may be null.
     * @param annotationCls The {@link Annotation} to check if is present on the method.
     * @param searchSupers  determines if a lookup in the entire inheritance hierarchy of the given class is performed if the annotation was not directly
     *                      present.
     * @param ignoreAccess  determines if underlying method has to be accessible.
     * @return The first matching annotation, or {@code null} if not found.
     * @throws NullPointerException Thrown if either the method or annotation class is {@code null}.
     * @throws SecurityException    Thrown if an underlying accessible object's method denies the request.
     * @see SecurityManager#checkPermission
     * @since 3.6
     */
    public static <A extends Annotation> A getAnnotation(final Method method, final Class<A> annotationCls, final boolean searchSupers,
            final boolean ignoreAccess) {
        Objects.requireNonNull(method, "method");
        Objects.requireNonNull(annotationCls, "annotationCls");
        if (!ignoreAccess && !MemberUtils.isAccessible(method)) {
            return null;
        }
        A annotation = method.getAnnotation(annotationCls);
        if (annotation == null && searchSupers) {
            final Class<?> mcls = method.getDeclaringClass();
            final String methodName = method.getName();
            final Class<?>[] paramTypes = method.getParameterTypes();
            final List<Class<?>> classes = getAllSuperclassesAndInterfaces(mcls);
            for (final Class<?> acls : classes) {
                // First, attempt an exact parameter-type match (getDeclaredMethod) to
                // find a true override. This avoids matching unrelated overloads that
                // are merely assignable-compatible (for example, process(Integer) versus
                // process(Number)).
                Method equivalentMethod = null;
                try {
                    equivalentMethod = acls.getDeclaredMethod(methodName, paramTypes);
                } catch (final NoSuchMethodException ignored) {
                    // No exact match; check for generic-bridge scenario: the declaring
                    // class may use a type variable whose erased form is Object (or
                    // another bound). In that case the parent method's erased
                    // parameter types differ from the child's concrete types, so we
                    // scan declared methods for a same-name method whose *erased*
                    // parameter count matches and whose erased types are assignable
                    // from our concrete types.
                    for (final Method candidate : acls.getDeclaredMethods()) {
                        if (!candidate.getName().equals(methodName)) {
                            continue;
                        }
                        final Class<?>[] candidateParams = candidate.getParameterTypes();
                        if (candidateParams.length != paramTypes.length) {
                            continue;
                        }
                        // Require that every concrete param type is assignable to the
                        // candidate's (erased) param type AND that the candidate is
                        // generic (has at least one TypeVariable in its generic
                        // parameter types). This prevents matching plain overloads.
                        boolean genericMatch = false;
                        boolean paramsMatch = true;
                        final java.lang.reflect.Type[] genericParams = candidate.getGenericParameterTypes();
                        for (int i = 0; i < candidateParams.length; i++) {
                            if (genericParams[i] instanceof java.lang.reflect.TypeVariable) {
                                genericMatch = true;
                            }
                            if (!ClassUtils.isAssignable(paramTypes[i], candidateParams[i], true)) {
                                paramsMatch = false;
                                break;
                            }
                        }
                        if (paramsMatch && genericMatch) {
                            equivalentMethod = candidate;
                            break;
                        }
                    }
                }
                if (equivalentMethod != null && (ignoreAccess || MemberUtils.isAccessible(equivalentMethod))) {
                    annotation = equivalentMethod.getAnnotation(annotationCls);
                    if (annotation != null) {
                        break;
                    }
                }
            }
        }
        return annotation;
    }

    private static Method getInvokeMethod(final boolean forceAccess, final String methodName, final Class<?>[] parameterTypes, final Class<?> cls) {
        final Method method;
        if (forceAccess) {
            method = getMatchingMethod(cls, methodName, parameterTypes);
            AccessibleObjects.setAccessible(method);
        } else {
            method = getMatchingAccessibleMethod(cls, methodName, parameterTypes);
        }
        return method;
    }

    private static Method getMatchingAccessibleMethod(final Class<?> cls, final String methodName, final boolean staticOnly,
            final Class<?>... requestTypes) {
        final Class<?>[] actualTypes = ArrayUtils.nullToEmpty(requestTypes);
        final Method candidate = getMethodObject(cls, methodName, actualTypes);
        if (candidate != null && (!staticOnly || MemberUtils.isStatic(candidate))) {
            // The exact match may be declared on a non-public class, so prefer the public
            // declaration the way the search below does; a static method hides, so it is kept.
            final Method accessibleCandidate = MemberUtils.isStatic(candidate) ? null : getAccessibleMethod(cls, candidate);
            return MemberUtils.setAccessibleWorkaround(accessibleCandidate != null ? accessibleCandidate : candidate);
        }
        // search through all methods
        final Method[] methods = cls.getMethods();
        final List<Method> matchingMethods = Stream.of(methods)
                .filter(method -> method.getName().equals(methodName) && (!staticOnly || MemberUtils.isStatic(method))
                        && MemberUtils.isMatchingMethod(method, actualTypes)).collect(Collectors.toList());
        // Sort methods by signature to force deterministic result
        matchingMethods.sort(METHOD_BY_SIGNATURE);
        Method bestMatch = null;
        for (final Method method : matchingMethods) {
            // get accessible version of method
            final Method accessibleMethod = getAccessibleMethod(method);
            if (accessibleMethod != null && (bestMatch == null || MemberUtils.compareMethodFit(accessibleMethod, bestMatch, actualTypes) < 0)) {
                bestMatch = accessibleMethod;
            }
        }
        return MemberUtils.setAccessibleWorkaround(bestMatch);
    }

    /**
     * Gets an accessible method that matches the given name and has compatible parameters. Compatible parameters mean that every method parameter is assignable
     * from the given parameters. In other words, it finds a method with the given name that will take the parameters given.
     *
     * <p>
     * This method is used by {@link #invokeMethod(Object object, String methodName, Object[] args, Class[] parameterTypes)}.
     * </p>
     * <p>
     * This method can match primitive parameter by passing in wrapper classes. For example, a {@link Boolean} will match a primitive {@code boolean} parameter.
     * </p>
     *
     * @param cls            find method in this class.
     * @param methodName     find method with this name.
     * @param requestTypes  The parameter types to match, {@code null} treated as empty.
     * @return The accessible method or null.
     * @throws SecurityException Thrown if an underlying accessible object's method denies the request.
     * @see SecurityManager#checkPermission
     */
    public static Method getMatchingAccessibleMethod(final Class<?> cls, final String methodName, final Class<?>... requestTypes) {
        return getMatchingAccessibleMethod(cls, methodName, false, requestTypes);
    }

    /**
     * Gets a method whether or not it's accessible. If no such method can be found, return {@code null}.
     *
     * @param cls            The class that will be subjected to the method search.
     * @param methodName     The method that we wish to call.
     * @param parameterTypes Argument class types, {@code null} treated as empty.
     * @throws IllegalStateException Thrown if there is no unique result.
     * @throws NullPointerException  Thrown if the class is {@code null}.
     * @return The method.
     * @since 3.5
     */
    public static Method getMatchingMethod(final Class<?> cls, final String methodName, final Class<?>... parameterTypes) {
        Objects.requireNonNull(cls, "cls");
        Validate.notEmpty(methodName, "methodName");
        final Class<?>[] actualTypes = ArrayUtils.nullToEmpty(parameterTypes);
        final List<Method> methods = Stream.of(cls.getDeclaredMethods())
                .filter(method -> method.getName().equals(methodName))
                .collect(Collectors.toList());
        final List<Class<?>> allSuperclassesAndInterfaces = getAllSuperclassesAndInterfaces(cls);
        Collections.reverse(allSuperclassesAndInterfaces);
        allSuperclassesAndInterfaces.stream()
                .map(Class::getDeclaredMethods)
                .flatMap(Stream::of)
                .filter(method -> method.getName().equals(methodName))
                .forEach(methods::add);
        for (final Method method : methods) {
            if (Arrays.deepEquals(method.getParameterTypes(), actualTypes)) {
                return method;
            }
        }
        final TreeMap<Integer, List<Method>> candidates = new TreeMap<>();
        methods.stream()
            .filter(method -> ClassUtils.isAssignable(actualTypes, method.getParameterTypes(), true))
            .forEach(method -> {
                 final int distance = distance(actualTypes, method.getParameterTypes());
                 final List<Method> candidatesAtDistance = candidates.computeIfAbsent(distance, k -> new ArrayList<>());
                 candidatesAtDistance.add(method);
        });
        if (candidates.isEmpty()) {
            return null;
        }
        final List<Method> bestCandidates = candidates.values().iterator().next();
        if (bestCandidates.size() == 1 || !Objects.equals(bestCandidates.get(0).getDeclaringClass(),
                bestCandidates.get(1).getDeclaringClass())) {
            return bestCandidates.get(0);
        }
        throw new IllegalStateException(String.format("Found multiple candidates for method %s on class %s : %s",
                methodName + Stream.of(actualTypes).map(String::valueOf).collect(Collectors.joining(",", "(", ")")), cls.getName(),
                bestCandidates.stream().map(Method::toString).collect(Collectors.joining(",", "[", "]"))));
    }

    /**
     * Gets a Method, or {@code null} if a checked {@link Class#getMethod(String, Class...) } exception is thrown.
     *
     * @param cls            Receiver for {@link Class#getMethod(String, Class...)}.
     * @param name           The name of the method.
     * @param parameterTypes The list of parameters.
     * @return A Method or {@code null}.
     * @see SecurityManager#checkPermission
     * @see Class#getMethod(String, Class...)
     * @since 3.15.0
     */
    public static Method getMethodObject(final Class<?> cls, final String name, final Class<?>... parameterTypes) {
        try {
            return name != null && cls != null ? cls.getMethod(name, parameterTypes) : null;
        } catch (final NoSuchMethodException | SecurityException e) {
            return null;
        }
    }

    /**
     * Gets all class level public methods of the given class that are annotated with the given annotation.
     *
     * @param cls           The {@link Class} to query.
     * @param annotationCls The {@link Annotation} that must be present on a method to be matched.
     * @return A list of Methods (possibly empty).
     * @throws NullPointerException Thrown if the class or annotation are {@code null}.
     * @since 3.4
     */
    public static List<Method> getMethodsListWithAnnotation(final Class<?> cls, final Class<? extends Annotation> annotationCls) {
        return getMethodsListWithAnnotation(cls, annotationCls, false, false);
    }

    /**
     * Gets all methods of the given class that are annotated with the given annotation.
     *
     * @param cls           The {@link Class} to query.
     * @param annotationCls The {@link Annotation} that must be present on a method to be matched.
     * @param searchSupers  determines if a lookup in the entire inheritance hierarchy of the given class should be performed.
     * @param ignoreAccess  determines if non-public methods should be considered.
     * @return A list of Methods (possibly empty).
     * @throws NullPointerException Thrown if either the class or annotation class is {@code null}.
     * @since 3.6
     */
    public static List<Method> getMethodsListWithAnnotation(final Class<?> cls, final Class<? extends Annotation> annotationCls, final boolean searchSupers,
            final boolean ignoreAccess) {
        Objects.requireNonNull(cls, "cls");
        Objects.requireNonNull(annotationCls, "annotationCls");
        final List<Class<?>> classes = searchSupers ? getAllSuperclassesAndInterfaces(cls) : new ArrayList<>();
        classes.add(0, cls);
        final List<Method> annotatedMethods = new ArrayList<>();
        classes.forEach(acls -> {
            final Method[] methods = ignoreAccess ? acls.getDeclaredMethods() : acls.getMethods();
            Stream.of(methods).filter(method -> method.isAnnotationPresent(annotationCls)).forEachOrdered(annotatedMethods::add);
        });
        return annotatedMethods;
    }

    /**
     * Gets all class level public methods of the given class that are annotated with the given annotation.
     *
     * @param cls           The {@link Class} to query.
     * @param annotationCls The {@link java.lang.annotation.Annotation} that must be present on a method to be matched.
     * @return An array of Methods (possibly empty).
     * @throws NullPointerException Thrown if the class or annotation are {@code null}.
     * @since 3.4
     */
    public static Method[] getMethodsWithAnnotation(final Class<?> cls, final Class<? extends Annotation> annotationCls) {
        return getMethodsWithAnnotation(cls, annotationCls, false, false);
    }

    /**
     * Gets all methods of the given class that are annotated with the given annotation.
     *
     * @param cls           The {@link Class} to query.
     * @param annotationCls The {@link java.lang.annotation.Annotation} that must be present on a method to be matched.
     * @param searchSupers  determines if a lookup in the entire inheritance hierarchy of the given class should be performed.
     * @param ignoreAccess  determines if non-public methods should be considered.
     * @return An array of Methods (possibly empty).
     * @throws NullPointerException Thrown if the class or annotation are {@code null}.
     * @since 3.6
     */
    public static Method[] getMethodsWithAnnotation(final Class<?> cls, final Class<? extends Annotation> annotationCls, final boolean searchSupers,
            final boolean ignoreAccess) {
        return getMethodsListWithAnnotation(cls, annotationCls, searchSupers, ignoreAccess).toArray(ArrayUtils.EMPTY_METHOD_ARRAY);
    }

    /**
     * Gets the hierarchy of overridden methods down to {@code result} respecting generics.
     *
     * @param method lowest to consider.
     * @param interfacesBehavior whether to search interfaces, {@code null} {@code implies} false.
     * @return A {@code Set<Method>} in ascending order from subclass to superclass.
     * @throws NullPointerException Thrown if the specified method is {@code null}.
     * @throws SecurityException Thrown if an underlying accessible object's method denies the request.
     * @see SecurityManager#checkPermission
     * @since 3.2
     */
    public static Set<Method> getOverrideHierarchy(final Method method, final Interfaces interfacesBehavior) {
        Objects.requireNonNull(method, "method");
        final Set<Method> result = new LinkedHashSet<>();
        result.add(method);
        final Class<?>[] parameterTypes = method.getParameterTypes();
        final Class<?> declaringClass = method.getDeclaringClass();
        final Iterator<Class<?>> hierarchy = ClassUtils.hierarchy(declaringClass, interfacesBehavior).iterator();
        //skip the declaring class :P
        hierarchy.next();
        hierarchyTraversal: while (hierarchy.hasNext()) {
            final Class<?> c = hierarchy.next();
            final Method m = getMatchingAccessibleMethod(c, method.getName(), parameterTypes);
            if (m == null) {
                continue;
            }
            if (Arrays.equals(m.getParameterTypes(), parameterTypes)) {
                // matches without generics
                result.add(m);
                continue;
            }
            // necessary to get arguments every time in the case that we are including interfaces
            final Map<TypeVariable<?>, Type> typeArguments = TypeUtils.getTypeArguments(declaringClass, m.getDeclaringClass());
            for (int i = 0; i < parameterTypes.length; i++) {
                final Type childType = TypeUtils.unrollVariables(typeArguments, method.getGenericParameterTypes()[i]);
                final Type parentType = TypeUtils.unrollVariables(typeArguments, m.getGenericParameterTypes()[i]);
                if (!TypeUtils.equals(childType, parentType)) {
                    continue hierarchyTraversal;
                }
            }
            result.add(m);
        }
        return result;
    }

    /**
     * Invokes a method whose parameter types match exactly the object type.
     *
     * <p>
     * This uses reflection to invoke the method obtained from a call to {@link #getAccessibleMethod(Class, String, Class[])}.
     * </p>
     *
     * @param object     invoke method on this object.
     * @param methodName get method with this name.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException       Thrown if there is no such accessible method.
     * @throws IllegalAccessException      Thrown if this found {@code Method} is enforcing Java language access control and the underlying method is
     *                                     inaccessible.
     * @throws IllegalArgumentException    Thrown if:
     *                                     <ul>
     *                                     <li>the found {@code Method} is an instance method and the specified {@code object} argument is not an instance of
     *                                     the class or interface declaring the underlying method (or of a subclass or interface implementor);</li>
     *                                     <li>the number of actual and formal parameters differ;</li>
     *                                     </ul>
     * @throws InvocationTargetException   Thrown if the underlying method throws an exception.
     * @throws NullPointerException        Thrown if the specified {@code object} is null.
     * @throws ExceptionInInitializerError Thrown if the initialization provoked by this method fails.
     * @since 3.4
     */
    public static Object invokeExactMethod(final Object object, final String methodName)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        return invokeExactMethod(object, methodName, ArrayUtils.EMPTY_OBJECT_ARRAY, null);
    }

    /**
     * Invokes a method whose parameter types match exactly the object types.
     *
     * <p>
     * This uses reflection to invoke the method obtained from a call to {@link #getAccessibleMethod(Class, String, Class[])}.
     * </p>
     *
     * @param object     invoke method on this object.
     * @param methodName get method with this name.
     * @param args       use these arguments - treat null as empty array.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException       Thrown if there is no such accessible method.
     * @throws IllegalAccessException      Thrown if this found {@code Method} is enforcing Java language access control and the underlying method is
     *                                     inaccessible.
     * @throws IllegalArgumentException    Thrown if:
     *                                     <ul>
     *                                     <li>the found {@code Method} is an instance method and the specified {@code object} argument is not an instance of
     *                                     the class or interface declaring the underlying method (or of a subclass or interface implementor);</li>
     *                                     <li>the number of actual and formal parameters differ;</li>
     *                                     <li>an unwrapping conversion for primitive arguments fails; or</li>
     *                                     <li>after possible unwrapping, a parameter value can't be converted to the corresponding formal parameter type by a
     *                                     method invocation conversion.</li>
     *                                     </ul>
     * @throws InvocationTargetException   Thrown if the underlying method throws an exception.
     * @throws NullPointerException        Thrown if the specified {@code object} is null.
     * @throws ExceptionInInitializerError Thrown if the initialization provoked by this method fails.
     */
    public static Object invokeExactMethod(final Object object, final String methodName, final Object... args)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Object[] actuals = ArrayUtils.nullToEmpty(args);
        return invokeExactMethod(object, methodName, actuals, ClassUtils.toClass(actuals));
    }

    /**
     * Invokes a method whose parameter types match exactly the parameter types given.
     *
     * <p>
     * This uses reflection to invoke the method obtained from a call to {@link #getAccessibleMethod(Class, String, Class[])}.
     * </p>
     *
     * @param object         Invokes a method on this object.
     * @param methodName     Gets a method with this name.
     * @param args           Method arguments - treat null as empty array.
     * @param parameterTypes Match these parameters - treat {@code null} as empty array.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException       Thrown if there is no such accessible method.
     * @throws IllegalAccessException      Thrown if this found {@code Method} is enforcing Java language access control and the underlying method is
     *                                     inaccessible.
     * @throws IllegalArgumentException    Thrown if:
     *                                     <ul>
     *                                     <li>the found {@code Method} is an instance method and the specified {@code object} argument is not an instance of
     *                                     the class or interface declaring the underlying method (or of a subclass or interface implementor);</li>
     *                                     <li>the number of actual and formal parameters differ;</li>
     *                                     <li>an unwrapping conversion for primitive arguments fails; or</li>
     *                                     <li>after possible unwrapping, a parameter value can't be converted to the corresponding formal parameter type by a
     *                                     method invocation conversion.</li>
     *                                     </ul>
     * @throws InvocationTargetException   Thrown if the underlying method throws an exception.
     * @throws NullPointerException        Thrown if the specified {@code object} is null.
     * @throws ExceptionInInitializerError Thrown if the initialization provoked by this method fails.
     */
    public static Object invokeExactMethod(final Object object, final String methodName, final Object[] args, final Class<?>[] parameterTypes)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Class<?> cls = Objects.requireNonNull(object, "object").getClass();
        final Class<?>[] paramTypes = ArrayUtils.nullToEmpty(parameterTypes);
        final Method method = getAccessibleMethod(cls, methodName, paramTypes);
        requireNonNull(method, cls, methodName, paramTypes);
        return method.invoke(object, ArrayUtils.nullToEmpty(args));
    }

    /**
     * Invokes a {@code static} method whose parameter types match exactly the object types.
     *
     * <p>
     * This uses reflection to invoke the method obtained from a call to {@link #getAccessibleMethod(Class, String, Class[])}.
     * The method must be static; a compatible overload with a different signature is not considered.
     * </p>
     *
     * @param cls        invoke static method on this class.
     * @param methodName get method with this name.
     * @param args       use these arguments - treat {@code null} as empty array.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException       Thrown if there is no accessible static method with the exact parameter types.
     * @throws IllegalAccessException      Thrown if this found {@code Method} is enforcing Java language access control and the underlying method is
     *                                     inaccessible.
     * @throws IllegalArgumentException    Thrown if:
     *                                     <ul>
     *                                     <li>the number of actual and formal parameters differ;</li>
     *                                     <li>an unwrapping conversion for primitive arguments fails; or</li>
     *                                     <li>after possible unwrapping, a parameter value can't be converted to the corresponding formal parameter type by a
     *                                     method invocation conversion.</li>
     *                                     </ul>
     * @throws InvocationTargetException   Thrown if the underlying method throws an exception.
     * @throws ExceptionInInitializerError Thrown if the initialization provoked by this method fails.
     */
    public static Object invokeExactStaticMethod(final Class<?> cls, final String methodName, final Object... args)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Object[] actuals = ArrayUtils.nullToEmpty(args);
        return invokeExactStaticMethod(cls, methodName, actuals, ClassUtils.toClass(actuals));
    }

    /**
     * Invokes a {@code static} method whose parameter types match exactly the parameter types given.
     *
     * <p>
     * This uses reflection to invoke the method obtained from a call to {@link #getAccessibleMethod(Class, String, Class[])}.
     * The method must be static; a compatible overload with a different signature is not considered.
     * </p>
     *
     * @param cls            invoke static method on this class.
     * @param methodName     get method with this name.
     * @param args           use these arguments - treat {@code null} as empty array.
     * @param parameterTypes match these parameters - treat {@code null} as empty array.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException       Thrown if there is no accessible static method with the exact parameter types.
     * @throws IllegalAccessException      Thrown if this found {@code Method} is enforcing Java language access control and the underlying method is
     *                                     inaccessible.
     * @throws IllegalArgumentException    Thrown if:
     *                                     <ul>
     *                                     <li>the number of actual and formal parameters differ;</li>
     *                                     <li>an unwrapping conversion for primitive arguments fails; or</li>
     *                                     <li>after possible unwrapping, a parameter value can't be converted to the corresponding formal parameter type by a
     *                                     method invocation conversion.</li>
     *                                     </ul>
     * @throws InvocationTargetException   Thrown if the underlying method throws an exception.
     * @throws ExceptionInInitializerError Thrown if the initialization provoked by this method fails.
     */
    public static Object invokeExactStaticMethod(final Class<?> cls, final String methodName, final Object[] args, final Class<?>[] parameterTypes)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Class<?>[] paramTypes = ArrayUtils.nullToEmpty(parameterTypes);
        final Method candidate = getAccessibleMethod(cls, methodName, ArrayUtils.nullToEmpty(paramTypes));
        final Method method = requireNonNull(candidate != null && MemberUtils.isStatic(candidate) ? candidate : null, cls, methodName, paramTypes);
        return method.invoke(null, ArrayUtils.nullToEmpty(args));
    }

    /**
     * Invokes a named method without parameters.
     *
     * <p>
     * This is a convenient wrapper for
     * {@link #invokeMethod(Object object, boolean forceAccess, String methodName, Object[] args, Class[] parameterTypes)}.
     * </p>
     *
     * @param object invoke method on this object.
     * @param forceAccess force access to invoke method even if it's not accessible.
     * @param methodName get method with this name.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException       Thrown if there is no such accessible method.
     * @throws IllegalAccessException      Thrown if this found {@code Method} is enforcing Java language access control and the underlying method is
     *                                     inaccessible.
     * @throws IllegalArgumentException    Thrown if:
     *                                     <ul>
     *                                     <li>the found {@code Method} is an instance method and the specified {@code object} argument is not an instance of
     *                                     the class or interface declaring the underlying method (or of a subclass or interface implementor);</li>
     *                                     <li>the number of actual and formal parameters differ;</li>
     *                                     <li>an unwrapping conversion for primitive arguments fails; or</li>
     *                                     <li>after possible unwrapping, a parameter value can't be converted to the corresponding formal parameter type by a
     *                                     method invocation conversion.</li>
     *                                     </ul>
     * @throws InvocationTargetException   Thrown if the underlying method throws an exception.
     * @throws NullPointerException        Thrown if the specified {@code object} is null.
     * @throws ExceptionInInitializerError Thrown if the initialization provoked by this method fails.
     * @see SecurityManager#checkPermission
     * @since 3.5
     */
    public static Object invokeMethod(final Object object, final boolean forceAccess, final String methodName)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        return invokeMethod(object, forceAccess, methodName, ArrayUtils.EMPTY_OBJECT_ARRAY, null);
    }

    /**
     * Invokes a named method whose parameter type matches the object type.
     *
     * <p>
     * This method supports calls to methods taking primitive parameters
     * via passing in wrapping classes. So, for example, a {@link Boolean} object
     * would match a {@code boolean} primitive.
     * </p>
     * <p>
     * This is a convenient wrapper for
     * {@link #invokeMethod(Object object, boolean forceAccess, String methodName, Object[] args, Class[] parameterTypes)}.
     * </p>
     *
     * @param object invoke method on this object.
     * @param forceAccess force access to invoke method even if it's not accessible.
     * @param methodName get method with this name.
     * @param args use these arguments - treat null as empty array.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException       Thrown if there is no such accessible method.
     * @throws IllegalAccessException      Thrown if this found {@code Method} is enforcing Java language access control and the underlying method is
     *                                     inaccessible.
     * @throws IllegalArgumentException    Thrown if:
     *                                     <ul>
     *                                     <li>the found {@code Method} is an instance method and the specified {@code object} argument is not an instance of
     *                                     the class or interface declaring the underlying method (or of a subclass or interface implementor);</li>
     *                                     <li>the number of actual and formal parameters differ;</li>
     *                                     <li>an unwrapping conversion for primitive arguments fails; or</li>
     *                                     <li>after possible unwrapping, a parameter value can't be converted to the corresponding formal parameter type by a
     *                                     method invocation conversion.</li>
     *                                     </ul>
     * @throws InvocationTargetException   Thrown if the underlying method throws an exception.
     * @throws NullPointerException        Thrown if the specified {@code object} is null.
     * @throws ExceptionInInitializerError Thrown if the initialization provoked by this method fails.
     * @see SecurityManager#checkPermission
     * @since 3.5
     */
    public static Object invokeMethod(final Object object, final boolean forceAccess, final String methodName, final Object... args)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Object[] actuals = ArrayUtils.nullToEmpty(args);
        return invokeMethod(object, forceAccess, methodName, actuals, ClassUtils.toClass(actuals));
    }

    /**
     * Invokes a named method whose parameter type matches the object type.
     *
     * <p>
     * This method supports calls to methods taking primitive parameters
     * via passing in wrapping classes. So, for example, a {@link Boolean} object
     * would match a {@code boolean} primitive.
     * </p>
     *
     * @param object invoke method on this object.
     * @param forceAccess force access to invoke method even if it's not accessible.
     * @param methodName get method with this name.
     * @param args use these arguments - treat null as empty array.
     * @param parameterTypes argument types for method selection and variable arity packing, {@code null} treated as empty.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException       Thrown if there is no such accessible method.
     * @throws IllegalAccessException      Thrown if this found {@code Method} is enforcing Java language access control and the underlying method is
     *                                     inaccessible.
     * @throws IllegalArgumentException    Thrown if:
     *                                     <ul>
     *                                     <li>the found {@code Method} is an instance method and the specified {@code object} argument is not an instance of
     *                                     the class or interface declaring the underlying method (or of a subclass or interface implementor);</li>
     *                                     <li>the number of actual and formal parameters differ;</li>
     *                                     <li>an unwrapping conversion for primitive arguments fails; or</li>
     *                                     <li>after possible unwrapping, a parameter value can't be converted to the corresponding formal parameter type by a
     *                                     method invocation conversion.</li>
     *                                     </ul>
     * @throws InvocationTargetException   Thrown if the underlying method throws an exception.
     * @throws NullPointerException        Thrown if the specified {@code object} is null.
     * @throws ExceptionInInitializerError Thrown if the initialization provoked by this method fails.
     * @see SecurityManager#checkPermission
     * @since 3.5
     */
    public static Object invokeMethod(final Object object, final boolean forceAccess, final String methodName, final Object[] args, final Class<?>[] parameterTypes)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Class<?> cls = Objects.requireNonNull(object, "object").getClass();
        final Class<?>[] paramTypes = ArrayUtils.nullToEmpty(parameterTypes);
        final Method method = getInvokeMethod(forceAccess, methodName, paramTypes, cls);
        requireNonNull(method, cls, methodName, paramTypes);
        return method.invoke(object, toVarArgs(method, ArrayUtils.nullToEmpty(args), paramTypes));
    }

    /**
     * Invokes a named method without parameters.
     *
     * <p>
     * This method delegates the method search to {@link #getMatchingAccessibleMethod(Class, String, Class[])}.
     * </p>
     * <p>
     * This is a convenient wrapper for
     * {@link #invokeMethod(Object object, String methodName, Object[] args, Class[] parameterTypes)}.
     * </p>
     *
     * @param object invoke method on this object.
     * @param methodName get method with this name.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException Thrown if there is no such accessible method.
     * @throws InvocationTargetException Thrown to wrap an exception thrown by the method invoked.
     * @throws IllegalAccessException Thrown if the requested method is not accessible via reflection.
     * @throws SecurityException Thrown if an underlying accessible object's method denies the request.
     * @see SecurityManager#checkPermission
     * @since 3.4
     */
    public static Object invokeMethod(final Object object, final String methodName) throws NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {
        return invokeMethod(object, methodName, ArrayUtils.EMPTY_OBJECT_ARRAY, null);
    }

    /**
     * Invokes a named method whose parameter type matches the object type.
     *
     * <p>
     * This method delegates the method search to {@link #getMatchingAccessibleMethod(Class, String, Class[])}.
     * </p>
     * <p>
     * This method supports calls to methods taking primitive parameters
     * via passing in wrapping classes. So, for example, a {@link Boolean} object
     * would match a {@code boolean} primitive.
     * </p>
     * <p>
     * This is a convenient wrapper for
     * {@link #invokeMethod(Object object, String methodName, Object[] args, Class[] parameterTypes)}.
     * </p>
     *
     * @param object invoke method on this object.
     * @param methodName get method with this name.
     * @param args use these arguments - treat null as empty array.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException Thrown if there is no such accessible method.
     * @throws InvocationTargetException Thrown to wrap an exception thrown by the method invoked.
     * @throws IllegalAccessException Thrown if the requested method is not accessible via reflection.
     * @throws NullPointerException Thrown if the object or method name are {@code null}.
     * @throws SecurityException Thrown if an underlying accessible object's method denies the request.
     * @see SecurityManager#checkPermission
     */
    public static Object invokeMethod(final Object object, final String methodName, final Object... args)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Object[] actuals = ArrayUtils.nullToEmpty(args);
        return invokeMethod(object, methodName, actuals, ClassUtils.toClass(actuals));
    }

    /**
     * Invokes a named method whose parameter type matches the object type.
     *
     * <p>
     * This method delegates the method search to {@link #getMatchingAccessibleMethod(Class, String, Class[])}.
     * </p>
     * <p>
     * This method supports calls to methods taking primitive parameters
     * via passing in wrapping classes. So, for example, a {@link Boolean} object
     * would match a {@code boolean} primitive.
     * </p>
     *
     * @param object invoke method on this object.
     * @param methodName get method with this name.
     * @param args use these arguments - treat null as empty array.
     * @param parameterTypes argument types for method selection and variable arity packing, {@code null} treated as empty.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException       Thrown if there is no such accessible method.
     * @throws IllegalAccessException      Thrown if this found {@code Method} is enforcing Java language access control and the underlying method is
     *                                     inaccessible.
     * @throws IllegalArgumentException    Thrown if:
     *                                     <ul>
     *                                     <li>the found {@code Method} is an instance method and the specified {@code object} argument is not an instance of
     *                                     the class or interface declaring the underlying method (or of a subclass or interface implementor);</li>
     *                                     <li>the number of actual and formal parameters differ;</li>
     *                                     <li>an unwrapping conversion for primitive arguments fails; or</li>
     *                                     <li>after possible unwrapping, a parameter value can't be converted to the corresponding formal parameter type by a
     *                                     method invocation conversion.</li>
     *                                     </ul>
     * @throws InvocationTargetException   Thrown if the underlying method throws an exception.
     * @throws NullPointerException        Thrown if the specified {@code object} is null.
     * @throws ExceptionInInitializerError Thrown if the initialization provoked by this method fails.
     * @see SecurityManager#checkPermission
     */
    public static Object invokeMethod(final Object object, final String methodName, final Object[] args, final Class<?>[] parameterTypes)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        return invokeMethod(object, false, methodName, args, parameterTypes);
    }

    /**
     * Invokes a named {@code static} method whose parameter type matches the object type.
     *
     * <p>
     * This method uses the matching rules of {@link #getMatchingAccessibleMethod(Class, String, Class[])}, but considers only static methods.
     * </p>
     *
     * <p>
     * This method supports calls to methods taking primitive parameters
     * via passing in wrapping classes. So, for example, a {@link Boolean} class
     * would match a {@code boolean} primitive.
     * </p>
     *
     * <p>
     * This is a convenient wrapper for
     * {@link #invokeStaticMethod(Class, String, Object[], Class[])}.
     * </p>
     *
     * @param cls invoke static method on this class.
     * @param methodName get method with this name.
     * @param args use these arguments - treat {@code null} as empty array.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException       Thrown if there is no matching accessible static method.
     * @throws IllegalAccessException      Thrown if this found {@code Method} is enforcing Java language access control and the underlying method is
     *                                     inaccessible.
     * @throws IllegalArgumentException    Thrown if:
     *                                     <ul>
     *                                     <li>the number of actual and formal parameters differ;</li>
     *                                     <li>an unwrapping conversion for primitive arguments fails; or</li>
     *                                     <li>after possible unwrapping, a parameter value can't be converted to the corresponding formal parameter type by a
     *                                     method invocation conversion.</li>
     *                                     </ul>
     * @throws InvocationTargetException   Thrown if the underlying method throws an exception.
     * @throws ExceptionInInitializerError Thrown if the initialization provoked by this method fails.
     * @see SecurityManager#checkPermission
     */
    public static Object invokeStaticMethod(final Class<?> cls, final String methodName, final Object... args)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Object[] actuals = ArrayUtils.nullToEmpty(args);
        return invokeStaticMethod(cls, methodName, actuals, ClassUtils.toClass(actuals));
    }

    /**
     * Invokes a named {@code static} method whose parameter type matches the object type.
     *
     * <p>
     * This method uses the matching rules of {@link #getMatchingAccessibleMethod(Class, String, Class[])}, but considers only static methods.
     * </p>
     *
     * <p>
     * This method supports calls to methods taking primitive parameters
     * via passing in wrapping classes. So, for example, a {@link Boolean} class
     * would match a {@code boolean} primitive.
     * </p>
     *
     * @param cls invoke static method on this class.
     * @param methodName get method with this name.
     * @param args use these arguments - treat {@code null} as empty array.
     * @param parameterTypes argument types for method selection and variable arity packing, {@code null} treated as empty.
     * @return The value returned by the invoked method.
     * @throws NoSuchMethodException       Thrown if there is no matching accessible static method.
     * @throws IllegalAccessException      Thrown if this found {@code Method} is enforcing Java language access control and the underlying method is
     *                                     inaccessible.
     * @throws IllegalArgumentException    Thrown if:
     *                                     <ul>
     *                                     <li>the number of actual and formal parameters differ;</li>
     *                                     <li>an unwrapping conversion for primitive arguments fails; or</li>
     *                                     <li>after possible unwrapping, a parameter value can't be converted to the corresponding formal parameter type by a
     *                                     method invocation conversion.</li>
     *                                     </ul>
     * @throws InvocationTargetException   Thrown if the underlying method throws an exception.
     * @throws ExceptionInInitializerError Thrown if the initialization provoked by this method fails.
     * @see SecurityManager#checkPermission
     */
    public static Object invokeStaticMethod(final Class<?> cls, final String methodName, final Object[] args, final Class<?>[] parameterTypes)
            throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        final Class<?>[] paramTypes = ArrayUtils.nullToEmpty(parameterTypes);
        final Method method = getMatchingAccessibleMethod(cls, methodName, true, paramTypes);
        requireNonNull(method, cls, methodName, paramTypes);
        return method.invoke(null, toVarArgs(method, ArrayUtils.nullToEmpty(args), paramTypes));
    }

    private static Method requireNonNull(final Method method, final Class<?> cls, final String methodName, final Class<?>[] parameterTypes)
            throws NoSuchMethodException {
        if (method == null) {
            throw new NoSuchMethodException(String.format("No method: %s.%s(%s)", ClassUtils.getName(cls), methodName,
                    Streams.of(parameterTypes).map(ClassUtils::getName).collect(LangCollectors.joining(", "))));
        }
        return method;
    }

    /**
     * Gets the arguments for an invocation, collecting variable arity arguments when necessary.
     *
     * @param executable The method or constructor to invoke.
     * @param args The non-null array of argument values.
     * @param parameterTypes The non-null array of supplied or inferred argument types.
     * @return The invocation arguments.
     * @throws IllegalArgumentException Thrown if an argument cannot be stored in the variable arity array.
     */
    static Object[] toVarArgs(final Executable executable, final Object[] args, final Class<?>[] parameterTypes) {
        if (!executable.isVarArgs()) {
            return args;
        }
        final Class<?>[] methodParameterTypes = executable.getParameterTypes();
        final int fixedArgCount = methodParameterTypes.length - 1;
        if (args.length == methodParameterTypes.length && ClassUtils.isAssignable(parameterTypes, methodParameterTypes, true)
                && (args[fixedArgCount] == null || methodParameterTypes[fixedArgCount].isInstance(args[fixedArgCount]))) {
            return args;
        }
        final Object[] newArgs = ArrayUtils.arraycopy(args, 0, 0, fixedArgCount, () -> new Object[methodParameterTypes.length]);
        final Class<?> componentType = methodParameterTypes[fixedArgCount].getComponentType();
        final int varArgCount = args.length - fixedArgCount;
        final Object varArgs = Array.newInstance(componentType, varArgCount);
        for (int i = 0; i < varArgCount; i++) {
            // Array.set preserves reference values and performs unboxing and primitive widening when needed.
            Array.set(varArgs, i, args[fixedArgCount + i]);
        }
        newArgs[fixedArgCount] = varArgs;
        return newArgs;
    }

    /**
     * {@link MethodUtils} instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code MethodUtils.getAccessibleMethod(method)}.
     *
     * <p>
     * This constructor is {@code public} to permit tools that require a JavaBean instance to operate.
     * </p>
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public MethodUtils() {
        // empty
    }
}
