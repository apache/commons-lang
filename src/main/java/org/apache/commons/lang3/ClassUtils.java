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

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.mutable.MutableObject;

/**
 * Operates on classes without using reflection.
 *
 * <p>
 * This class handles invalid {@code null} inputs as best it can. Each method documents its behavior in more detail.
 * </p>
 *
 * <p>
 * The notion of a {@code canonical name} includes the human readable name for the type, for example {@code int[]}. The
 * non-canonical method variants work with the JVM names, such as {@code [I}.
 * </p>
 *
 * @since 2.0
 */
public class ClassUtils {

    /**
     * Inclusivity literals for {@link #hierarchy(Class, Interfaces)}.
     *
     * @since 3.2
     */
    public enum Interfaces {

        /** Includes interfaces. */
        INCLUDE,

        /** Excludes interfaces. */
        EXCLUDE
    }

    private static final Comparator<Class<?>> COMPARATOR = (o1, o2) -> Objects.compare(getName(o1), getName(o2), String::compareTo);

    /**
     * The package separator character: {@code '&#x2e;' == {@value}}.
     */
    public static final char PACKAGE_SEPARATOR_CHAR = '.';

    /**
     * The package separator String: {@code "&#x2e;"}.
     */
    public static final String PACKAGE_SEPARATOR = String.valueOf(PACKAGE_SEPARATOR_CHAR);

    /**
     * The inner class separator character: {@code '$' == {@value}}.
     */
    public static final char INNER_CLASS_SEPARATOR_CHAR = '$';

    /**
     * The inner class separator String: {@code "$"}.
     */
    public static final String INNER_CLASS_SEPARATOR = String.valueOf(INNER_CLASS_SEPARATOR_CHAR);

    /**
     * Maps names of primitives to their corresponding primitive {@link Class}es.
     */
    private static final Map<String, Class<?>> namePrimitiveMap = new HashMap<>();

    static {
        namePrimitiveMap.put("boolean", Boolean.TYPE);
        namePrimitiveMap.put("byte", Byte.TYPE);
        namePrimitiveMap.put("char", Character.TYPE);
        namePrimitiveMap.put("short", Short.TYPE);
        namePrimitiveMap.put("int", Integer.TYPE);
        namePrimitiveMap.put("long", Long.TYPE);
        namePrimitiveMap.put("double", Double.TYPE);
        namePrimitiveMap.put("float", Float.TYPE);
        namePrimitiveMap.put("void", Void.TYPE);
    }

    /**
     * Maps primitive {@link Class}es to their corresponding wrapper {@link Class}.
     */
    private static final Map<Class<?>, Class<?>> primitiveWrapperMap = new HashMap<>();

    static {
        primitiveWrapperMap.put(Boolean.TYPE, Boolean.class);
        primitiveWrapperMap.put(Byte.TYPE, Byte.class);
        primitiveWrapperMap.put(Character.TYPE, Character.class);
        primitiveWrapperMap.put(Short.TYPE, Short.class);
        primitiveWrapperMap.put(Integer.TYPE, Integer.class);
        primitiveWrapperMap.put(Long.TYPE, Long.class);
        primitiveWrapperMap.put(Double.TYPE, Double.class);
        primitiveWrapperMap.put(Float.TYPE, Float.class);
        primitiveWrapperMap.put(Void.TYPE, Void.TYPE);
    }

    /**
     * Maps wrapper {@link Class}es to their corresponding primitive types.
     */
    private static final Map<Class<?>, Class<?>> wrapperPrimitiveMap = new HashMap<>();

    static {
        primitiveWrapperMap.forEach((primitiveClass, wrapperClass) -> {
            if (!primitiveClass.equals(wrapperClass)) {
                wrapperPrimitiveMap.put(wrapperClass, primitiveClass);
            }
        });
    }

    /**
     * Maps a primitive class name to its corresponding abbreviation used in array class names.
     */
    private static final Map<String, String> abbreviationMap;

    /**
     * Maps an abbreviation used in array class names to corresponding primitive class name.
     */
    private static final Map<String, String> reverseAbbreviationMap;

    /** Feed abbreviation maps. */
    static {
        final Map<String, String> map = new HashMap<>();
        map.put("int", "I");
        map.put("boolean", "Z");
        map.put("float", "F");
        map.put("long", "J");
        map.put("short", "S");
        map.put("byte", "B");
        map.put("double", "D");
        map.put("char", "C");
        abbreviationMap = Collections.unmodifiableMap(map);
        reverseAbbreviationMap = Collections.unmodifiableMap(map.entrySet().stream().collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey)));
    }

    /**
     * Gets the class comparator, comparing by class name.
     *
     * @return the class comparator.
     * @since 3.13.0
     */
    public static Comparator<Class<?>> comparator() {
        return COMPARATOR;
    }

    /**
     * Given a {@link List} of {@link Class} objects, this method converts them into class names.
     *
     * <p>
     * A new {@link List} is returned. {@code null} objects will be copied into the returned list as {@code null}.
     * </p>
     *
     * @param classes the classes to change
     * @return a {@link List} of class names corresponding to the Class objects, {@code null} if null input
     * @throws ClassCastException if {@code classes} contains a non-{@link Class} entry
     */
    public static List<String> convertClassesToClassNames(final List<Class<?>> classes) {
        return classes == null ? null : classes.stream().map(e -> getName(e, null)).collect(Collectors.toList());
    }

    /**
     * Given a {@link List} of class names, this method converts them into classes.
     *
     * <p>
     * A new {@link List} is returned. If the class name cannot be found, {@code null} is stored in the {@link List}. If the
     * class name in the {@link List} is {@code null}, {@code null} is stored in the output {@link List}.
     * </p>
     *
     * @param classNames the classNames to change
     * @return a {@link List} of Class objects corresponding to the class names, {@code null} if null input
     * @throws ClassCastException if classNames contains a non String entry
     */
    public static List<Class<?>> convertClassNamesToClasses(final List<String> classNames) {
        if (classNames == null) {
            return null;
        }
        final List<Class<?>> classes = new ArrayList<>(classNames.size());
        classNames.forEach(className -> {
            try {
                classes.add(Class.forName(className));
            } catch (final Exception ex) {
                classes.add(null);
            }
        });
        return classes;
    }

    /**
     * Gets the abbreviated name of a {@link Class}.
     *
     * @param cls the class to get the abbreviated name for, may be {@code null}
     * @param lengthHint the desired length of the abbreviated name
     * @return the abbreviated name or an empty string
     * @throws IllegalArgumentException if len &lt;= 0
     * @see #getAbbreviatedName(String, int)
     * @since 3.4
     */
    public static String getAbbreviatedName(final Class<?> cls, final int lengthHint) {
        if (cls == null) {
            return StringUtils.EMPTY;
        }
        return getAbbreviatedName(cls.getName(), lengthHint);
    }

    /**
     * Gets the abbreviated class name from a {@link String}.
     *
     * <p>
     * The string passed in is assumed to be a class name - it is not checked.
     * </p>
     *
     * <p>
     * The abbreviation algorithm will shorten the class name, usually without significant loss of meaning.
     * </p>
     *
     * <p>
     * The abbreviated class name will always include the complete package hierarchy. If enough space is available,
     * rightmost sub-packages will be displayed in full length. The abbreviated package names will be shortened to a single
     * character.
     * </p>
     * <p>
     * Only package names are shortened, the class simple name remains untouched. (See examples.)
     * </p>
     * <p>
     * The result will be longer than the desired length only if all the package names shortened to a single character plus
     * the class simple name with the separating dots together are longer than the desired length. In other words, when the
     * class name cannot be shortened to the desired length.
     * </p>
     * <p>
     * If the class name can be shortened then the final length will be at most {@code lengthHint} characters.
     * </p>
     * <p>
     * If the {@code lengthHint} is zero or negative then the method throws exception. If you want to achieve the shortest
     * possible version then use {@code 1} as a {@code lengthHint}.
     * </p>
     *
     * <table>
     * <caption>Examples</caption>
     * <tr>
     * <td>className</td>
     * <td>len</td>
     * <td>return</td>
     * </tr>
     * <tr>
     * <td>null</td>
     * <td>1</td>
     * <td>""</td>
     * </tr>
     * <tr>
     * <td>"java.lang.String"</td>
     * <td>5</td>
     * <td>"j.l.String"</td>
     * </tr>
     * <tr>
     * <td>"java.lang.String"</td>
     * <td>15</td>
     * <td>"j.lang.String"</td>
     * </tr>
     * <tr>
     * <td>"java.lang.String"</td>
     * <td>30</td>
     * <td>"java.lang.String"</td>
     * </tr>
     * <tr>
     * <td>"org.apache.commons.lang3.ClassUtils"</td>
     * <td>18</td>
     * <td>"o.a.c.l.ClassUtils"</td>
     * </tr>
     * </table>
     *
     * @param className the className to get the abbreviated name for, may be {@code null}
     * @param lengthHint the desired length of the abbreviated name
     * @return the abbreviated name or an empty string if the specified class name is {@code null} or empty string. The
     *         abbreviated name may be longer than the desired length if it cannot be abbreviated to the desired length.
     * @throws IllegalArgumentException if {@code len <= 0}
     * @since 3.4
     */
    public static String getAbbreviatedName(final String className, final int lengthHint) {
        if (lengthHint <= 0) {
            throw new IllegalArgumentException("len must be > 0");
        }
        if (className == null) {
            return StringUtils.EMPTY;
        }
        if (className.length() <= lengthHint) {
            return className;
        }
        final char[] abbreviated = className.toCharArray();
        int target = 0;
        int source = 0;
        while (source < abbreviated.length) {
            // copy the next part
            int runAheadTarget = target;
            while (source < abbreviated.length && abbreviated[source] != '.') {
                abbreviated[runAheadTarget++] = abbreviated[source++];
            }

            ++target;
            if (useFull(runAheadTarget, source, abbreviated.length, lengthHint) || target > runAheadTarget) {
                target = runAheadTarget;
            }

            // copy the '.' unless it was the last part
            if (source < abbreviated.length) {
                abbreviated[target++] = abbreviated[source++];
            }
        }
        return new String(abbreviated, 0, target);
    }

    /**
     * Gets a {@link List} of all interfaces implemented by the given class and its superclasses.
     *
     * <p>
     * The order is determined by looking through each interface in turn as declared in the source file and following its
     * hierarchy up. Then each superclass is considered in the same way. Later duplicates are ignored, so the order is
     * maintained.
     * </p>
     *
     * @param cls the class to look up, may be {@code null}
     * @return the {@link List} of interfaces in order, {@code null} if null input
     */
    public static List<Class<?>> getAllInterfaces(final Class<?> cls) {
        if (cls == null) {
            return null;
        }

        final LinkedHashSet<Class<?>> interfacesFound = new LinkedHashSet<>();
        getAllInterfaces(cls, interfacesFound);

        return new ArrayList<>(interfacesFound);
    }

    /**
     * Gets the interfaces for the specified class.
     *
     * @param cls the class to look up, may be {@code null}
     * @param interfacesFound the {@link Set} of interfaces for the class
     */
    private static void getAllInterfaces(Class<?> cls, final HashSet<Class<?>> interfacesFound) {
        while (cls != null) {
            final Class<?>[] interfaces = cls.getInterfaces();

            for (final Class<?> i : interfaces) {
                if (interfacesFound.add(i)) {
                    getAllInterfaces(i, interfacesFound);
                }
            }

            cls = cls.getSuperclass();
        }
    }

    /**
     * Gets a {@link List} of superclasses for the given class.
     *
     * @param cls the class to look up, may be {@code null}
     * @return the {@link List} of superclasses in order going up from this one {@code null} if null input
     */
    public static List<Class<?>> getAllSuperclasses(final Class<?> cls) {
        if (cls == null) {
            return null;
        }
        final List<Class<?>> classes = new ArrayList<>();
        Class<?> superclass = cls.getSuperclass();
        while (superclass != null) {
            classes.add(superclass);
            superclass = superclass.getSuperclass();
        }
        return classes;
    }

    /**
     * Gets the canonical class name for a {@link Class}.
     *
     * @param cls the class for which to get the canonical class name; may be null
     * @return the canonical name of the class, or the empty String
     * @since 3.7
     * @see Class#getCanonicalName()
     */
    public static String getCanonicalName(final Class<?> cls) {
        return getCanonicalName(cls, StringUtils.EMPTY);
    }

    /**
     * Gets the canonical name for a {@link Class}.
     *
     * @param cls the class for which to get the canonical class name; may be null
     * @param valueIfNull the return value if null
     * @return the canonical name of the class, or {@code valueIfNull}
     * @since 3.7
     * @see Class#getCanonicalName()
     */
    public static String getCanonicalName(final Class<?> cls, final String valueIfNull) {
        if (cls == null) {
            return valueIfNull;
        }
        final String canonicalName = cls.getCanonicalName();
        return canonicalName == null ? valueIfNull : canonicalName;
    }

    /**
     * Gets the canonical name for an {@link Object}.
     *
     * @param object the object for which to get the canonical class name; may be null
     * @return the canonical name of the object, or the empty String
     * @since 3.7
     * @see Class#getCanonicalName()
     */
    public static String getCanonicalName(final Object object) {
        return getCanonicalName(object, StringUtils.EMPTY);
    }

    /**
     * Gets the canonical name for an {@link Object}.
     *
     * @param object the object for which to get the canonical class name; may be null
     * @param valueIfNull the return value if null
     * @return the canonical name of the object or {@code valueIfNull}
     * @since 3.7
     * @see Class#getCanonicalName()
     */
    public static String getCanonicalName(final Object object, final String valueIfNull) {
        if (object == null) {
            return valueIfNull;
        }
        final String canonicalName = object.getClass().getCanonicalName();
        return canonicalName == null ? valueIfNull : canonicalName;
    }

    /**
     * Converts a given name of class into canonical format. If name of class is not a name of array class it returns
     * unchanged name.
     *
     * <p>
     * The method does not change the {@code $} separators in case the class is inner class.
     * </p>
     *
     * <p>
     * Example:
     * <ul>
     * <li>{@code getCanonicalName("[I") = "int[]"}</li>
     * <li>{@code getCanonicalName("[Ljava.lang.String;") = "java.lang.String[]"}</li>
     * <li>{@code getCanonicalName("java.lang.String") = "java.lang.String"}</li>
     * </ul>
     * </p>
     *
     * @param className the name of class
     * @return canonical form of class name
     * @since 2.4
     */
    private static String getCanonicalName(String className) {
        className = StringUtils.deleteWhitespace(className);
        if (className == null) {
            return null;
        }
        int dim = 0;
        while (className.startsWith("[")) {
            dim++;
            className = className.substring(1);
        }
        if (dim < 1) {
            return className;
        }
        if (className.startsWith("L")) {
            className = className.substring(1, className.endsWith(";") ? className.length() - 1 : className.length());
        } else if (!className.isEmpty()) {
            className = reverseAbbreviationMap.get(className.substring(0, 1));
        }
        final StringBuilder canonicalClassNameBuffer = new StringBuilder(className);
        for (int i = 0; i < dim; i++) {
            canonicalClassNameBuffer.append("[]");
        }
        return canonicalClassNameBuffer.toString();
    }

    /**
     * Returns the (initialized) class represented by {@code className} using the {@code classLoader}. This implementation
     * supports the syntaxes "{@code java.util.Map.Entry[]}", "{@code java.util.Map$Entry[]}",
     * "{@code [Ljava.util.Map.Entry;}", and "{@code [Ljava.util.Map$Entry;}".
     *
     * @param classLoader the class loader to use to load the class
     * @param className the class name
     * @return the class represented by {@code className} using the {@code classLoader}
     * @throws NullPointerException if the className is null
     * @throws ClassNotFoundException if the class is not found
     */
    public static Class<?> getClass(final ClassLoader classLoader, final String className) throws ClassNotFoundException {
        return getClass(classLoader, className, true);
    }

    /**
     * Returns the class represented by {@code className} using the {@code classLoader}. This implementation supports the
     * syntaxes "{@code java.util.Map.Entry[]}", "{@code java.util.Map$Entry[]}", "{@code [Ljava.util.Map.Entry;}", and
     * "{@code [Ljava.util.Map$Entry;}".
     *
     * @param classLoader the class loader to use to load the class
     * @param className the class name
     * @param initialize whether the class must be initialized
     * @return the class represented by {@code className} using the {@code classLoader}
     * @throws NullPointerException if the className is null
     * @throws ClassNotFoundException if the class is not found
     */
    public static Class<?> getClass(final ClassLoader classLoader, final String className, final boolean initialize) throws ClassNotFoundException {
        try {
            Class<?> clazz = namePrimitiveMap.get(className);
            return clazz != null ? clazz : Class.forName(toCanonicalName(className), initialize, classLoader);
        } catch (final ClassNotFoundException ex) {
            // allow path separators (.) as inner class name separators
            final int lastDotIndex = className.lastIndexOf(PACKAGE_SEPARATOR_CHAR);

            if (lastDotIndex != -1) {
                try {
                    return getClass(classLoader, className.substring(0, lastDotIndex) + INNER_CLASS_SEPARATOR_CHAR + className.substring(lastDotIndex + 1),
                        initialize);
                } catch (final ClassNotFoundException ignored) {
                    // ignore exception
                }
            }

            throw ex;
        }
    }

    /**
     * Returns the (initialized) class represented by {@code className} using the current thread's context class loader.
     * This implementation supports the syntaxes "{@code java.util.Map.Entry[]}", "{@code java.util.Map$Entry[]}",
     * "{@code [Ljava.util.Map.Entry;}", and "{@code [Ljava.util.Map$Entry;}".
     *
     * @param className the class name
     * @return the class represented by {@code className} using the current thread's context class loader
     * @throws NullPointerException if the className is null
     * @throws ClassNotFoundException if the class is not found
     */
    public static Class<?> getClass(final String className) throws ClassNotFoundException {
        return getClass(className, true);
    }

    /**
     * Returns the class represented by {@code className} using the current thread's context class loader. This
     * implementation supports the syntaxes "{@code java.util.Map.Entry[]}", "{@code java.util.Map$Entry[]}",
     * "{@code [Ljava.util.Map.Entry;}", and "{@code [Ljava.util.Map$Entry;}".
     *
     * @param className the class name
     * @param initialize whether the class must be initialized
     * @return the class represented by {@code className} using the current thread's context class loader
     * @throws NullPointerException if the className is null
     * @throws ClassNotFoundException if the class is not found
     */
    public static Class<?> getClass(final String className, final boolean initialize) throws ClassNotFoundException {
        final ClassLoader contextCL = Thread.currentThread().getContextClassLoader();
        final ClassLoader loader = contextCL == null ? ClassUtils.class.getClassLoader() : contextCL;
        return getClass(loader, className, initialize);
    }

    /**
     * Delegates to {@link Class#getComponentType()} using generics.
     *
     * @param <T> The array class type.
     * @param cls A class or null.
     * @return The array component type or null.
     * @see Class#getComponentType()
     * @since 3.13.0
     */
    @SuppressWarnings("unchecked")
    public static <T> Class<T> getComponentType(final Class<T[]> cls) {
        return cls == null ? null : (Class<T>) cls.getComponentType();
    }

    /**
     * Null-safe version of {@code cls.getName()}
     *
     * @param cls the class for which to get the class name; may be null
     * @return the class name or the empty string in case the argument is {@code null}
     * @since 3.7
     * @see Class#getSimpleName()
     */
    public static String getName(final Class<?> cls) {
        return getName(cls, StringUtils.EMPTY);
    }

    /**
     * Null-safe version of {@code cls.getName()}
     *
     * @param cls the class for which to get the class name; may be null
     * @param valueIfNull the return value if the argument {@code cls} is {@code null}
     * @return the class name or {@code valueIfNull}
     * @since 3.7
     * @see Class#getName()
     */
    public static String getName(final Class<?> cls, final String valueIfNull) {
        return cls == null ? valueIfNull : cls.getName();
    }

    /**
     * Null-safe version of {@code object.getClass().getName()}
     *
     * @param object the object for which to get the class name; may be null
     * @return the class name or the empty String
     * @since 3.7
     * @see Class#getSimpleName()
     */
    public static String getName(final Object object) {
        return getName(object, StringUtils.EMPTY);
    }

    /**
     * Null-safe version of {@code object.getClass().getSimpleName()}
     *
     * @param object the object for which to get the class name; may be null
     * @param valueIfNull the value to return if {@code object} is {@code null}
     * @return the class name or {@code valueIfNull}
     * @since 3.0
     * @see Class#getName()
     */
    public static String getName(final Object object, final String valueIfNull) {
        return object == null ? valueIfNull : object.getClass().getName();
    }

    /**
     * Gets the package name from the canonical name of a {@link Class}.
     *
     * @param cls the class to get the package name for, may be {@code null}.
     * @return the package name or an empty string
     * @since 2.4
     */
    public static String getPackageCanonicalName(final Class<?> cls) {
        if (cls == null) {
            return StringUtils.EMPTY;
        }
        return getPackageCanonicalName(cls.getName());
    }

    /**
     * Gets the package name from the class name of an {@link Object}.
     *
     * @param object the class to get the package name for, may be null
     * @param valueIfNull the value to return if null
     * @return the package name of the object, or the null value
     * @since 2.4
     */
    public static String getPackageCanonicalName(final Object object, final String valueIfNull) {
        if (object == null) {
            return valueIfNull;
        }
        return getPackageCanonicalName(object.getClass().getName());
    }

    /**
     * Gets the package name from the class name.
     *
     * <p>
     * The string passed in is assumed to be a class name - it is not checked.
     * </p>
     * <p>
     * If the class is in the default package, return an empty string.
     * </p>
     *
     * @param name the name to get the package name for, may be {@code null}
     * @return the package name or an empty string
     * @since 2.4
     */
    public static String getPackageCanonicalName(final String name) {
        return getPackageName(getCanonicalName(name));
    }

    /**
     * Gets the package name of a {@link Class}.
     *
     * @param cls the class to get the package name for, may be {@code null}.
     * @return the package name or an empty string
     */
    public static String getPackageName(final Class<?> cls) {
        if (cls == null) {
            return StringUtils.EMPTY;
        }
        return getPackageName(cls.getName());
    }

    /**
     * Gets the package name of an {@link Object}.
     *
     * @param object the class to get the package name for, may be null
     * @param valueIfNull the value to return if null
     * @return the package name of the object, or the null value
     */
    public static String getPackageName(final Object object, final String valueIfNull) {
        if (object == null) {
            return valueIfNull;
        }
        return getPackageName(object.getClass());
    }

    /**
     * Gets the package name from a {@link String}.
     *
     * <p>
     * The string passed in is assumed to be a class name - it is not checked.
     * </p>
     * <p>
     * If the class is unpackaged, return an empty string.
     * </p>
     *
     * @param className the className to get the package name for, may be {@code null}
     * @return the package name or an empty string
     */
    public static String getPackageName(String className) {
        if (StringUtils.isEmpty(className)) {
            return StringUtils.EMPTY;
        }

        // Strip array encoding
        while (className.charAt(0) == '[') {
            className = className.substring(1);
        }
        // Strip Object type encoding
        if (className.charAt(0) == 'L' && className.charAt(className.length() - 1) == ';') {
            className = className.substring(1);
        }

        final int i = className.lastIndexOf(PACKAGE_SEPARATOR_CHAR);
        if (i == -1) {
            return StringUtils.EMPTY;
        }
        return className.substring(0, i);
    }

    /**
     * Returns the desired Method much like {@code Class.getMethod}, however it ensures that the returned Method is from a
     * public class or interface and not from an anonymous inner class. This means that the Method is invokable and doesn't
     * fall foul of Java bug <a href="https://bugs.java.com/bugdatabase/view_bug.do?bug_id=4071957">4071957</a>).
     *
     * <pre>
     *  <code>Set set = Collections.unmodifiableSet(...);
     *  Method method = ClassUtils.getPublicMethod(set.getClass(), "isEmpty",  new Class[0]);
     *  Object result = method.invoke(set, new Object[]);</code>
     * </pre>
     *
     * @param cls the class to check, not null
     * @param methodName the name of the method
     * @param parameterTypes the list of parameters
     * @return the method
     * @throws NullPointerException if the class is null
     * @throws SecurityException if a security violation occurred
     * @throws NoSuchMethodException if the method is not found in the given class or if the method doesn't conform with the
     *         requirements
     */
    public static Method getPublicMethod(final Class<?> cls, final String methodName, final Class<?>... parameterTypes) throws NoSuchMethodException {

        final Method declaredMethod = cls.getMethod(methodName, parameterTypes);
        if (isPublic(declaredMethod.getDeclaringClass())) {
            return declaredMethod;
        }

        final List<Class<?>> candidateClasses = new ArrayList<>(getAllInterfaces(cls));
        candidateClasses.addAll(getAllSuperclasses(cls));

        for (final Class<?> candidateClass : candidateClasses) {
            if (!isPublic(candidateClass)) {
                continue;
            }
            final Method candidateMethod;
            try {
                candidateMethod = candidateClass.getMethod(methodName, parameterTypes);
            } catch (final NoSuchMethodException ex) {
                continue;
            }
            if (Modifier.isPublic(candidateMethod.getDeclaringClass().getModifiers())) {
                return candidateMethod;
            }
        }

        throw new NoSuchMethodException("Can't find a public method for " + methodName + " " + ArrayUtils.toString(parameterTypes));
    }

    /**
     * Gets the canonical name minus the package name from a {@link Class}.
     *
     * @param cls the class for which to get the short canonical class name; may be null
     * @return the canonical name without the package name or an empty string
     * @since 2.4
     * @see Class#getCanonicalName()
     */
    public static String getShortCanonicalName(final Class<?> cls) {
        return cls == null ? StringUtils.EMPTY : getShortCanonicalName(cls.getCanonicalName());
    }

    /**
     * Gets the canonical name minus the package name for an {@link Object}.
     *
     * @param object the class to get the short name for, may be null
     * @param valueIfNull the value to return if null
     * @return the canonical name of the object without the package name, or the null value
     * @since 2.4
     * @see Class#getCanonicalName()
     */
    public static String getShortCanonicalName(final Object object, final String valueIfNull) {
        return object == null ? valueIfNull : getShortCanonicalName(object.getClass().getCanonicalName());
    }

    /**
     * Gets the canonical name minus the package name from a String.
     *
     * <p>
     * The string passed in is assumed to be a class name - it is not checked.
     * </p>
     *
     * <p>
     * Note that this method is mainly designed to handle the arrays and primitives properly. If the class is an inner class
     * then the result value will not contain the outer classes. This way the behavior of this method is different from
     * {@link #getShortClassName(String)}. The argument in that case is class name and not canonical name and the return
     * value retains the outer classes.
     * </p>
     *
     * <p>
     * Note that there is no way to reliably identify the part of the string representing the package hierarchy and the part
     * that is the outer class or classes in case of an inner class. Trying to find the class would require reflective call
     * and the class itself may not even be on the class path. Relying on the fact that class names start with capital
     * letter and packages with lower case is heuristic.
     * </p>
     *
     * <p>
     * It is recommended to use {@link #getShortClassName(String)} for cases when the class is an inner class and use this
     * method for cases it is designed for.
     * </p>
     *
     * <table>
     * <caption>Examples</caption>
     * <tr>
     * <td>return value</td>
     * <td>input</td>
     * </tr>
     * <tr>
     * <td>{@code ""}</td>
     * <td>{@code (String)null}</td>
     * </tr>
     * <tr>
     * <td>{@code "Map.Entry"}</td>
     * <td>{@code java.util.Map.Entry.class.getName()}</td>
     * </tr>
     * <tr>
     * <td>{@code "Entry"}</td>
     * <td>{@code java.util.Map.Entry.class.getCanonicalName()}</td>
     * </tr>
     * <tr>
     * <td>{@code "ClassUtils"}</td>
     * <td>{@code "org.apache.commons.lang3.ClassUtils"}</td>
     * </tr>
     * <tr>
     * <td>{@code "ClassUtils[]"}</td>
     * <td>{@code "[Lorg.apache.commons.lang3.ClassUtils;"}</td>
     * </tr>
     * <tr>
     * <td>{@code "ClassUtils[][]"}</td>
     * <td>{@code "[[Lorg.apache.commons.lang3.ClassUtils;"}</td>
     * </tr>
     * <tr>
     * <td>{@code "ClassUtils[]"}</td>
     * <td>{@code "org.apache.commons.lang3.ClassUtils[]"}</td>
     * </tr>
     * <tr>
     * <td>{@code "ClassUtils[][]"}</td>
     * <td>{@code "org.apache.commons.lang3.ClassUtils[][]"}</td>
     * </tr>
     * <tr>
     * <td>{@code "int[]"}</td>
     * <td>{@code "[I"}</td>
     * </tr>
     * <tr>
     * <td>{@code "int[]"}</td>
     * <td>{@code int[].class.getCanonicalName()}</td>
     * </tr>
     * <tr>
     * <td>{@code "int[]"}</td>
     * <td>{@code int[].class.getName()}</td>
     * </tr>
     * <tr>
     * <td>{@code "int[][]"}</td>
     * <td>{@code "[[I"}</td>
     * </tr>
     * <tr>
     * <td>{@code "int[]"}</td>
     * <td>{@code "int[]"}</td>
     * </tr>
     * <tr>
     * <td>{@code "int[][]"}</td>
     * <td>{@code "int[][]"}</td>
     * </tr>
     * </table>
     *
     * @param canonicalName the class name to get the short name for
     * @return the canonical name of the class without the package name or an empty string
     * @since 2.4
     */
    public static String getShortCanonicalName(final String canonicalName) {
        return getShortClassName(getCanonicalName(canonicalName));
    }

    /**
     * Gets the class name minus the package name from a {@link Class}.
     *
     * <p>
     * This method simply gets the name using {@code Class.getName()} and then calls {@link #getShortClassName(Class)}. See
     * relevant notes there.
     * </p>
     *
     * @param cls the class to get the short name for.
     * @return the class name without the package name or an empty string. If the class is an inner class then the returned
     *         value will contain the outer class or classes separated with {@code .} (dot) character.
     */
    public static String getShortClassName(final Class<?> cls) {
        if (cls == null) {
            return StringUtils.EMPTY;
        }
        return getShortClassName(cls.getName());
    }

    /**
     * Gets the class name of the {@code object} without the package name or names.
     *
     * <p>
     * The method looks up the class of the object and then converts the name of the class invoking
     * {@link #getShortClassName(Class)} (see relevant notes there).
     * </p>
     *
     * @param object the class to get the short name for, may be {@code null}
     * @param valueIfNull the value to return if the object is {@code null}
     * @return the class name of the object without the package name, or {@code valueIfNull} if the argument {@code object}
     *         is {@code null}
     */
    public static String getShortClassName(final Object object, final String valueIfNull) {
        if (object == null) {
            return valueIfNull;
        }
        return getShortClassName(object.getClass());
    }

    /**
     * Gets the class name minus the package name from a String.
     *
     * <p>
     * The string passed in is assumed to be a class name - it is not checked. The string has to be formatted the way as the
     * JDK method {@code Class.getName()} returns it, and not the usual way as we write it, for example in import
     * statements, or as it is formatted by {@code Class.getCanonicalName()}.
     * </p>
     *
     * <p>
     * The difference is is significant only in case of classes that are inner classes of some other classes. In this case
     * the separator between the outer and inner class (possibly on multiple hierarchy level) has to be {@code $} (dollar
     * sign) and not {@code .} (dot), as it is returned by {@code Class.getName()}
     * </p>
     *
     * <p>
     * Note that this method is called from the {@link #getShortClassName(Class)} method using the string returned by
     * {@code Class.getName()}.
     * </p>
     *
     * <p>
     * Note that this method differs from {@link #getSimpleName(Class)} in that this will return, for example
     * {@code "Map.Entry"} whilst the {@code java.lang.Class} variant will simply return {@code "Entry"}. In this example
     * the argument {@code className} is the string {@code java.util.Map$Entry} (note the {@code $} sign.
     * </p>
     *
     * @param className the className to get the short name for. It has to be formatted as returned by
     *        {@code Class.getName()} and not {@code Class.getCanonicalName()}
     * @return the class name of the class without the package name or an empty string. If the class is an inner class then
     *         value contains the outer class or classes and the separator is replaced to be {@code .} (dot) character.
     */
    public static String getShortClassName(String className) {
        if (StringUtils.isEmpty(className)) {
            return StringUtils.EMPTY;
        }

        final StringBuilder arrayPrefix = new StringBuilder();

        // Handle array encoding
        if (className.startsWith("[")) {
            while (className.charAt(0) == '[') {
                className = className.substring(1);
                arrayPrefix.append("[]");
            }
            // Strip Object type encoding
            if (className.charAt(0) == 'L' && className.charAt(className.length() - 1) == ';') {
                className = className.substring(1, className.length() - 1);
            }

            if (reverseAbbreviationMap.containsKey(className)) {
                className = reverseAbbreviationMap.get(className);
            }
        }

        final int lastDotIdx = className.lastIndexOf(PACKAGE_SEPARATOR_CHAR);
        final int innerIdx = className.indexOf(INNER_CLASS_SEPARATOR_CHAR, lastDotIdx == -1 ? 0 : lastDotIdx + 1);
        String out = className.substring(lastDotIdx + 1);
        if (innerIdx != -1) {
            out = out.replace(INNER_CLASS_SEPARATOR_CHAR, PACKAGE_SEPARATOR_CHAR);
        }
        return out + arrayPrefix;
    }

    /**
     * Null-safe version of {@code cls.getSimpleName()}
     *
     * @param cls the class for which to get the simple name; may be null
     * @return the simple class name or the empty string in case the argument is {@code null}
     * @since 3.0
     * @see Class#getSimpleName()
     */
    public static String getSimpleName(final Class<?> cls) {
        return getSimpleName(cls, StringUtils.EMPTY);
    }

    /**
     * Null-safe version of {@code cls.getSimpleName()}
     *
     * @param cls the class for which to get the simple name; may be null
     * @param valueIfNull the value to return if null
     * @return the simple class name or {@code valueIfNull} if the argument {@code cls} is {@code null}
     * @since 3.0
     * @see Class#getSimpleName()
     */
    public static String getSimpleName(final Class<?> cls, final String valueIfNull) {
        return cls == null ? valueIfNull : cls.getSimpleName();
    }

    /**
     * Null-safe version of {@code object.getClass().getSimpleName()}
     *
     * <p>
     * It is to note that this method is overloaded and in case the argument {@code object} is a {@link Class} object then
     * the {@link #getSimpleName(Class)} will be invoked. If this is a significant possibility then the caller should check
     * this case and call {@code
     * getSimpleName(Class.class)} or just simply use the string literal {@code "Class"}, which is the result of the method
     * in that case.
     * </p>
     *
     * @param object the object for which to get the simple class name; may be null
     * @return the simple class name or the empty string in case the argument is {@code null}
     * @since 3.7
     * @see Class#getSimpleName()
     */
    public static String getSimpleName(final Object object) {
        return getSimpleName(object, StringUtils.EMPTY);
    }

    /**
     * Null-safe version of {@code object.getClass().getSimpleName()}
     *
     * @param object the object for which to get the simple class name; may be null
     * @param valueIfNull the value to return if {@code object} is {@code null}
     * @return the simple class name or {@code valueIfNull} if the argument {@code object} is {@code null}
     * @since 3.0
     * @see Class#getSimpleName()
     */
    public static String getSimpleName(final Object object, final String valueIfNull) {
        return object == null ? valueIfNull : object.getClass().getSimpleName();
    }

    /**
     * Gets an {@link Iterable} that can iterate over a class hierarchy in ascending (subclass to superclass) order,
     * excluding interfaces.
     *
     * @param type the type to get the class hierarchy from
     * @return Iterable an Iterable over the class hierarchy of the given class
     * @since 3.2
     */
    public static Iterable<Class<?>> hierarchy(final Class<?> type) {
        return hierarchy(type, Interfaces.EXCLUDE);
    }

    /**
     * Gets an {@link Iterable} that can iterate over a class hierarchy in ascending (subclass to superclass) order.
     *
     * @param type the type to get the class hierarchy from
     * @param interfacesBehavior switch indicating whether to include or exclude interfaces
     * @return Iterable an Iterable over the class hierarchy of the given class
     * @since 3.2
     */
    public static Iterable<Class<?>> hierarchy(final Class<?> type, final Interfaces interfacesBehavior) {
        final Iterable<Class<?>> classes = () -> {
            final MutableObject<Class<?>> next = new MutableObject<>(type);
            return new Iterator<Class<?>>() {

                @Override
                public boolean hasNext() {
                    return next.getValue() != null;
                }

                @Override
                public Class<?> next() {
                    final Class<?> result = next.getValue();
                    next.setValue(result.getSuperclass());
                    return result;
                }

                @Override
                public void remove() {
                    throw new UnsupportedOperationException();
                }

            };
        };
        if (interfacesBehavior != Interfaces.INCLUDE) {
            return classes;
        }
        return () -> {
            final Set<Class<?>> seenInterfaces = new HashSet<>();
            final Iterator<Class<?>> wrapped = classes.iterator();

            return new Iterator<Class<?>>() {
                Iterator<Class<?>> interfaces = Collections.emptyIterator();

                @Override
                public boolean hasNext() {
                    return interfaces.hasNext() || wrapped.hasNext();
                }

                @Override
                public Class<?> next() {
                    if (interfaces.hasNext()) {
                        final Class<?> nextInterface = interfaces.next();
                        seenInterfaces.add(nextInterface);
                        return nextInterface;
                    }
                    final Class<?> nextSuperclass = wrapped.next();
                    final Set<Class<?>> currentInterfaces = new LinkedHashSet<>();
                    walkInterfaces(currentInterfaces, nextSuperclass);
                    interfaces = currentInterfaces.iterator();
                    return nextSuperclass;
                }

                @Override
                public void remove() {
                    throw new UnsupportedOperationException();
                }

                private void walkInterfaces(final Set<Class<?>> addTo, final Class<?> c) {
                    for (final Class<?> iface : c.getInterfaces()) {
                        if (!seenInterfaces.contains(iface)) {
                            addTo.add(iface);
                        }
                        walkInterfaces(addTo, iface);
                    }
                }

            };
        };
    }

    /**
     * Checks if one {@link Class} can be assigned to a variable of another {@link Class}.
     *
     * <p>
     * Unlike the {@link Class#isAssignableFrom(java.lang.Class)} method, this method takes into account widenings of
     * primitive classes and {@code null}s.
     * </p>
     *
     * <p>
     * Primitive widenings allow an int to be assigned to a long, float or double. This method returns the correct result
     * for these cases.
     * </p>
     *
     * <p>
     * {@code null} may be assigned to any reference type. This method will return {@code true} if {@code null} is passed in
     * and the toClass is non-primitive.
     * </p>
     *
     * <p>
     * Specifically, this method tests whether the type represented by the specified {@link Class} parameter can be
     * converted to the type represented by this {@link Class} object via an identity conversion widening primitive or
     * widening reference conversion. See <em><a href="https://docs.oracle.com/javase/specs/">The Java Language
     * Specification</a></em>, sections 5.1.1, 5.1.2 and 5.1.4 for details.
     * </p>
     *
     * <p>
     * <strong>Since Lang 3.0,</strong> this method will default behavior for calculating assignability between primitive
     * and wrapper types <em>corresponding to the running Java version</em>; i.e. autoboxing will be the default behavior in
     * VMs running Java versions &gt; 1.5.
     * </p>
     *
     * @param cls the Class to check, may be null
     * @param toClass the Class to try to assign into, returns false if null
     * @return {@code true} if assignment possible
     */
    public static boolean isAssignable(final Class<?> cls, final Class<?> toClass) {
        return isAssignable(cls, toClass, true);
    }

    /**
     * Checks if one {@link Class} can be assigned to a variable of another {@link Class}.
     *
     * <p>
     * Unlike the {@link Class#isAssignableFrom(java.lang.Class)} method, this method takes into account widenings of
     * primitive classes and {@code null}s.
     * </p>
     *
     * <p>
     * Primitive widenings allow an int to be assigned to a long, float or double. This method returns the correct result
     * for these cases.
     * </p>
     *
     * <p>
     * {@code null} may be assigned to any reference type. This method will return {@code true} if {@code null} is passed in
     * and the toClass is non-primitive.
     * </p>
     *
     * <p>
     * Specifically, this method tests whether the type represented by the specified {@link Class} parameter can be
     * converted to the type represented by this {@link Class} object via an identity conversion widening primitive or
     * widening reference conversion. See <em><a href="https://docs.oracle.com/javase/specs/">The Java Language
     * Specification</a></em>, sections 5.1.1, 5.1.2 and 5.1.4 for details.
     * </p>
     *
     * @param cls the Class to check, may be null
     * @param toClass the Class to try to assign into, returns false if null
     * @param autoboxing whether to use implicit autoboxing/unboxing between primitives and wrappers
     * @return {@code true} if assignment possible
     */
    public static boolean isAssignable(Class<?> cls, final Class<?> toClass, final boolean autoboxing) {
        if (toClass == null) {
            return false;
        }
        // have to check for null, as isAssignableFrom doesn't
        if (cls == null) {
            return !toClass.isPrimitive();
        }
        // autoboxing:
        if (autoboxing) {
            if (cls.isPrimitive() && !toClass.isPrimitive()) {
                cls = primitiveToWrapper(cls);
                if (cls == null) {
                    return false;
                }
            }
            if (toClass.isPrimitive() && !cls.isPrimitive()) {
                cls = wrapperToPrimitive(cls);
                if (cls == null) {
                    return false;
                }
            }
        }
        if (cls.equals(toClass)) {
            return true;
        }
        if (cls.isPrimitive()) {
            if (!toClass.isPrimitive()) {
                return false;
            }
            if (Integer.TYPE.equals(cls)) {
                return Long.TYPE.equals(toClass) || Float.TYPE.equals(toClass) || Double.TYPE.equals(toClass);
            }
            if (Long.TYPE.equals(cls)) {
                return Float.TYPE.equals(toClass) || Double.TYPE.equals(toClass);
            }
            if (Boolean.TYPE.equals(cls)) {
                return false;
            }
            if (Double.TYPE.equals(cls)) {
                return false;
            }
            if (Float.TYPE.equals(cls)) {
                return Double.TYPE.equals(toClass);
            }
            if (Character.TYPE.equals(cls)  || Short.TYPE.equals(cls)) {
                return Integer.TYPE.equals(toClass) || Long.TYPE.equals(toClass) || Float.TYPE.equals(toClass) || Double.TYPE.equals(toClass);
            }
            if (Byte.TYPE.equals(cls)) {
                return Short.TYPE.equals(toClass) || Integer.TYPE.equals(toClass) || Long.TYPE.equals(toClass) || Float.TYPE.equals(toClass)
                    || Double.TYPE.equals(toClass);
            }
            // should never get here
            return false;
        }
        return toClass.isAssignableFrom(cls);
    }

    /**
     * Checks if an array of Classes can be assigned to another array of Classes.
     *
     * <p>
     * This method calls {@link #isAssignable(Class, Class) isAssignable} for each Class pair in the input arrays. It can be
     * used to check if a set of arguments (the first parameter) are suitably compatible with a set of method parameter
     * types (the second parameter).
     * </p>
     *
     * <p>
     * Unlike the {@link Class#isAssignableFrom(java.lang.Class)} method, this method takes into account widenings of
     * primitive classes and {@code null}s.
     * </p>
     *
     * <p>
     * Primitive widenings allow an int to be assigned to a {@code long}, {@code float} or {@code double}. This method
     * returns the correct result for these cases.
     * </p>
     *
     * <p>
     * {@code null} may be assigned to any reference type. This method will return {@code true} if {@code null} is passed in
     * and the toClass is non-primitive.
     * </p>
     *
     * <p>
     * Specifically, this method tests whether the type represented by the specified {@link Class} parameter can be
     * converted to the type represented by this {@link Class} object via an identity conversion widening primitive or
     * widening reference conversion. See <em><a href="https://docs.oracle.com/javase/specs/">The Java Language
     * Specification</a></em>, sections 5.1.1, 5.1.2 and 5.1.4 for details.
     * </p>
     *
     * <p>
     * <strong>Since Lang 3.0,</strong> this method will default behavior for calculating assignability between primitive
     * and wrapper types <em>corresponding to the running Java version</em>; i.e. autoboxing will be the default behavior in
     * VMs running Java versions &gt; 1.5.
     * </p>
     *
     * @param classArray the array of Classes to check, may be {@code null}
     * @param toClassArray the array of Classes to try to assign into, may be {@code null}
     * @return {@code true} if assignment possible
     */
    public static boolean isAssignable(final Class<?>[] classArray, final Class<?>... toClassArray) {
        return isAssignable(classArray, toClassArray, true);
    }

    /**
     * Checks if an array of Classes can be assigned to another array of Classes.
     *
     * <p>
     * This method calls {@link #isAssignable(Class, Class) isAssignable} for each Class pair in the input arrays. It can be
     * used to check if a set of arguments (the first parameter) are suitably compatible with a set of method parameter
     * types (the second parameter).
     * </p>
     *
     * <p>
     * Unlike the {@link Class#isAssignableFrom(java.lang.Class)} method, this method takes into account widenings of
     * primitive classes and {@code null}s.
     * </p>
     *
     * <p>
     * Primitive widenings allow an int to be assigned to a {@code long}, {@code float} or {@code double}. This method
     * returns the correct result for these cases.
     * </p>
     *
     * <p>
     * {@code null} may be assigned to any reference type. This method will return {@code true} if {@code null} is passed in
     * and the toClass is non-primitive.
     * </p>
     *
     * <p>
     * Specifically, this method tests whether the type represented by the specified {@link Class} parameter can be
     * converted to the type represented by this {@link Class} object via an identity conversion widening primitive or
     * widening reference conversion. See <em><a href="https://docs.oracle.com/javase/specs/">The Java Language
     * Specification</a></em>, sections 5.1.1, 5.1.2 and 5.1.4 for details.
     * </p>
     *
     * @param classArray the array of Classes to check, may be {@code null}
     * @param toClassArray the array of Classes to try to assign into, may be {@code null}
     * @param autoboxing whether to use implicit autoboxing/unboxing between primitives and wrappers
     * @return {@code true} if assignment possible
     */
    public static boolean isAssignable(Class<?>[] classArray, Class<?>[] toClassArray, final boolean autoboxing) {
        if (!ArrayUtils.isSameLength(classArray, toClassArray)) {
            return false;
        }
        if (classArray == null) {
            classArray = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        if (toClassArray == null) {
            toClassArray = ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        for (int i = 0; i < classArray.length; i++) {
            if (!isAssignable(classArray[i], toClassArray[i], autoboxing)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Is the specified class an inner class or static nested class.
     *
     * @param cls the class to check, may be null
     * @return {@code true} if the class is an inner or static nested class, false if not or {@code null}
     */
    public static boolean isInnerClass(final Class<?> cls) {
        return cls != null && cls.getEnclosingClass() != null;
    }

    /**
     * Tests whether a {@link Class} is public.
     * @param cls Class to test.
     * @return {@code true} if {@code cls} is public.
     * @since 3.13.0
     */
    public static boolean isPublic(final Class<?> cls) {
        return Modifier.isPublic(cls.getModifiers());
    }
    /**
     * Returns whether the given {@code type} is a primitive or primitive wrapper ({@link Boolean}, {@link Byte},
     * {@link Character}, {@link Short}, {@link Integer}, {@link Long}, {@link Double}, {@link Float}).
     *
     * @param type The class to query or null.
     * @return true if the given {@code type} is a primitive or primitive wrapper ({@link Boolean}, {@link Byte},
     *         {@link Character}, {@link Short}, {@link Integer}, {@link Long}, {@link Double}, {@link Float}).
     * @since 3.1
     */
    public static boolean isPrimitiveOrWrapper(final Class<?> type) {
        if (type == null) {
            return false;
        }
        return type.isPrimitive() || isPrimitiveWrapper(type);
    }

    /**
     * Returns whether the given {@code type} is a primitive wrapper ({@link Boolean}, {@link Byte}, {@link Character},
     * {@link Short}, {@link Integer}, {@link Long}, {@link Double}, {@link Float}).
     *
     * @param type The class to query or null.
     * @return true if the given {@code type} is a primitive wrapper ({@link Boolean}, {@link Byte}, {@link Character},
     *         {@link Short}, {@link Integer}, {@link Long}, {@link Double}, {@link Float}).
     * @since 3.1
     */
    public static boolean isPrimitiveWrapper(final Class<?> type) {
        return wrapperPrimitiveMap.containsKey(type);
    }

    /**
     * Converts the specified array of primitive Class objects to an array of its corresponding wrapper Class objects.
     *
     * @param classes the class array to convert, may be null or empty
     * @return an array which contains for each given class, the wrapper class or the original class if class is not a
     *         primitive. {@code null} if null input. Empty array if an empty array passed in.
     * @since 2.1
     */
    public static Class<?>[] primitivesToWrappers(final Class<?>... classes) {
        if (classes == null) {
            return null;
        }

        if (classes.length == 0) {
            return classes;
        }

        final Class<?>[] convertedClasses = new Class[classes.length];
        Arrays.setAll(convertedClasses, i -> primitiveToWrapper(classes[i]));
        return convertedClasses;
    }

    /**
     * Converts the specified primitive Class object to its corresponding wrapper Class object.
     *
     * <p>
     * NOTE: From v2.2, this method handles {@code Void.TYPE}, returning {@code Void.TYPE}.
     * </p>
     *
     * @param cls the class to convert, may be null
     * @return the wrapper class for {@code cls} or {@code cls} if {@code cls} is not a primitive. {@code null} if null
     *         input.
     * @since 2.1
     */
    public static Class<?> primitiveToWrapper(final Class<?> cls) {
        Class<?> convertedClass = cls;
        if (cls != null && cls.isPrimitive()) {
            convertedClass = primitiveWrapperMap.get(cls);
        }
        return convertedClass;
    }

    /**
     * Converts a class name to a JLS style class name.
     *
     * @param className the class name
     * @return the converted name
     * @throws NullPointerException if the className is null
     */
    private static String toCanonicalName(final String className) {
        String canonicalName = StringUtils.deleteWhitespace(className);
        Objects.requireNonNull(canonicalName, "className");
        if (canonicalName.endsWith("[]")) {
            final StringBuilder classNameBuffer = new StringBuilder();
            while (canonicalName.endsWith("[]")) {
                canonicalName = canonicalName.substring(0, canonicalName.length() - 2);
                classNameBuffer.append("[");
            }
            final String abbreviation = abbreviationMap.get(canonicalName);
            if (abbreviation != null) {
                classNameBuffer.append(abbreviation);
            } else {
                classNameBuffer.append("L").append(canonicalName).append(";");
            }
            canonicalName = classNameBuffer.toString();
        }
        return canonicalName;
    }

    /**
     * Converts an array of {@link Object} in to an array of {@link Class} objects. If any of these objects is null, a null
     * element will be inserted into the array.
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param array an {@link Object} array
     * @return a {@link Class} array, {@code null} if null array input
     * @since 2.4
     */
    public static Class<?>[] toClass(final Object... array) {
        if (array == null) {
            return null;
        }
        if (array.length == 0) {
            return ArrayUtils.EMPTY_CLASS_ARRAY;
        }
        final Class<?>[] classes = new Class[array.length];
        Arrays.setAll(classes, i -> array[i] == null ? null : array[i].getClass());
        return classes;
    }

    /**
     * Decides if the part that was just copied to its destination location in the work array can be kept as it was copied
     * or must be abbreviated. It must be kept when the part is the last one, which is the simple name of the class. In this
     * case the {@code source} index, from where the characters are copied points one position after the last character,
     * a.k.a. {@code source ==
     * originalLength}
     *
     * <p>
     * If the part is not the last one then it can be kept unabridged if the number of the characters copied so far plus the
     * character that are to be copied is less than or equal to the desired length.
     * </p>
     *
     * @param runAheadTarget the target index (where the characters were copied to) pointing after the last character copied
     *        when the current part was copied
     * @param source the source index (where the characters were copied from) pointing after the last character copied when
     *        the current part was copied
     * @param originalLength the original length of the class full name, which is abbreviated
     * @param desiredLength the desired length of the abbreviated class name
     * @return {@code true} if it can be kept in its original length {@code false} if the current part has to be abbreviated
     *         and
     */
    private static boolean useFull(final int runAheadTarget, final int source, final int originalLength, final int desiredLength) {
        return source >= originalLength || runAheadTarget + originalLength - source <= desiredLength;
    }

    /**
     * Converts the specified array of wrapper Class objects to an array of its corresponding primitive Class objects.
     *
     * <p>
     * This method invokes {@code wrapperToPrimitive()} for each element of the passed in array.
     * </p>
     *
     * @param classes the class array to convert, may be null or empty
     * @return an array which contains for each given class, the primitive class or <b>null</b> if the original class is not
     *         a wrapper class. {@code null} if null input. Empty array if an empty array passed in.
     * @see #wrapperToPrimitive(Class)
     * @since 2.4
     */
    public static Class<?>[] wrappersToPrimitives(final Class<?>... classes) {
        if (classes == null) {
            return null;
        }

        if (classes.length == 0) {
            return classes;
        }

        final Class<?>[] convertedClasses = new Class[classes.length];
        Arrays.setAll(convertedClasses, i -> wrapperToPrimitive(classes[i]));
        return convertedClasses;
    }

    /**
     * Converts the specified wrapper class to its corresponding primitive class.
     *
     * <p>
     * This method is the counter part of {@code primitiveToWrapper()}. If the passed in class is a wrapper class for a
     * primitive type, this primitive type will be returned (e.g. {@code Integer.TYPE} for {@code Integer.class}). For other
     * classes, or if the parameter is <b>null</b>, the return value is <b>null</b>.
     * </p>
     *
     * @param cls the class to convert, may be <b>null</b>
     * @return the corresponding primitive type if {@code cls} is a wrapper class, <b>null</b> otherwise
     * @see #primitiveToWrapper(Class)
     * @since 2.4
     */
    public static Class<?> wrapperToPrimitive(final Class<?> cls) {
        return wrapperPrimitiveMap.get(cls);
    }

    /**
     * ClassUtils instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code ClassUtils.getShortClassName(cls)}.
     *
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     */
    public ClassUtils() {
    }

}
