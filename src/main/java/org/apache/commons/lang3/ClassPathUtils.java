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

import java.util.Objects;

/**
 * Operations regarding the classpath.
 *
 * <p>
 * The methods of this class do not allow {@code null} inputs.
 * </p>
 *
 * @since 3.3
 */
//@Immutable
public class ClassPathUtils {

    /**
     * Gets the package name of the context class.
     * <p>
     * Returns an empty string for an unnamed package, including when {@link Class#getPackage()} returns {@code null} on Java 8.
     * </p>
     *
     * @param context The context class, must not be {@code null}.
     * @return The package name, or an empty string for an unnamed package.
     * @throws NullPointerException Thrown if {@code context} is null, represents an array, a primitive type or {@code void}, or represents a named class whose
     *                              package is unavailable.
     */
    private static String getPackageName(final Class<?> context) {
        final Package contextPackage = context.getPackage();
        if (contextPackage == null && !context.isArray() && !context.isPrimitive() && context.getName().indexOf('.') < 0) {
            return StringUtils.EMPTY;
        }
        return Objects.requireNonNull(contextPackage, "context").getName();
    }

    /**
     * Converts a package name to a Java path ('/').
     *
     * @param path The source path.
     * @return A package name.
     * @throws NullPointerException Thrown if {@code path} is null.
     * @since 3.13.0
     */
    public static String packageToPath(final String path) {
        return Objects.requireNonNull(path, "path").replace('.', '/');
    }

    /**
     * Converts a Java path ('/') to a package name.
     *
     * @param path The source path.
     * @return A package name.
     * @throws NullPointerException Thrown if {@code path} is null.
     * @since 3.13.0
     */
    public static String pathToPackage(final String path) {
        return Objects.requireNonNull(path, "path").replace('/', '.');
    }

    /**
     * Qualifies a resource name with a prefix and separator.
     * <p>
     * Returns {@code resourceName} unchanged when the prefix is empty.
     * </p>
     *
     * @param prefix       The package name or path, must not be {@code null}.
     * @param resourceName The resource name, validated as non-null by the caller.
     * @param separator    The separator between the prefix and resource name.
     * @return The qualified resource name, or the unchanged resource name when the prefix is empty.
     * @throws NullPointerException Thrown if {@code prefix} is null.
     */
    private static String qualify(final String prefix, final String resourceName, final char separator) {
        return prefix.isEmpty() ? resourceName : prefix + separator + resourceName;
    }

    /**
     * Returns the fully qualified name for the resource with name {@code resourceName} relative to the given context.
     *
     * <p>
     * Note that this method does not check whether the resource actually exists. It only constructs the name. Null inputs are not allowed.
     * </p>
     *
     * <p>
     * If the context class is in an unnamed package, returns {@code resourceName} unchanged.
     * </p>
     *
     * <pre>
     * ClassPathUtils.toFullyQualifiedName(StringUtils.class, "StringUtils.properties") = "org.apache.commons.lang3.StringUtils.properties"
     * </pre>
     *
     * @param context      The context for constructing the name.
     * @param resourceName The resource name to construct the fully qualified name for.
     * @return The fully qualified name of the resource with name {@code resourceName}.
     * @throws NullPointerException Thrown if either {@code context} or {@code resourceName} is null, if {@code context} represents an array, a primitive
     *                              type or {@code void}, or if it represents a named class whose package is unavailable.
     */
    public static String toFullyQualifiedName(final Class<?> context, final String resourceName) {
        Objects.requireNonNull(context, "context");
        Objects.requireNonNull(resourceName, "resourceName");
        return qualify(getPackageName(context), resourceName, '.');
    }

    /**
     * Returns the fully qualified name for the resource with name {@code resourceName} relative to the given context.
     *
     * <p>
     * Note that this method does not check whether the resource actually exists. It only constructs the name. Null inputs are not allowed.
     * </p>
     *
     * <p>
     * If the context is an unnamed package, returns {@code resourceName} unchanged.
     * </p>
     *
     * <pre>
     * ClassPathUtils.toFullyQualifiedName(StringUtils.class.getPackage(), "StringUtils.properties") = "org.apache.commons.lang3.StringUtils.properties"
     * </pre>
     *
     * @param context      The context for constructing the name.
     * @param resourceName The resource name to construct the fully qualified name for.
     * @return The fully qualified name of the resource with name {@code resourceName}.
     * @throws NullPointerException Thrown if either {@code context} or {@code resourceName} is null.
     */
    public static String toFullyQualifiedName(final Package context, final String resourceName) {
        Objects.requireNonNull(context, "context");
        Objects.requireNonNull(resourceName, "resourceName");
        return qualify(context.getName(), resourceName, '.');
    }

    /**
     * Returns the fully qualified path for the resource with name {@code resourceName} relative to the given context.
     *
     * <p>
     * Note that this method does not check whether the resource actually exists. It only constructs the path. Null inputs are not allowed.
     * </p>
     *
     * <p>
     * If the context class is in an unnamed package, returns {@code resourceName} unchanged.
     * </p>
     *
     * <pre>
     * ClassPathUtils.toFullyQualifiedPath(StringUtils.class, "StringUtils.properties") = "org/apache/commons/lang3/StringUtils.properties"
     * </pre>
     *
     * @param context      The context for constructing the path.
     * @param resourceName The resource name to construct the fully qualified path for.
     * @return The fully qualified path of the resource with name {@code resourceName}.
     * @throws NullPointerException Thrown if either {@code context} or {@code resourceName} is null, if {@code context} represents an array, a primitive
     *                              type or {@code void}, or if it represents a named class whose package is unavailable.
     */
    public static String toFullyQualifiedPath(final Class<?> context, final String resourceName) {
        Objects.requireNonNull(context, "context");
        Objects.requireNonNull(resourceName, "resourceName");
        return qualify(packageToPath(getPackageName(context)), resourceName, '/');
    }

    /**
     * Returns the fully qualified path for the resource with name {@code resourceName} relative to the given context.
     *
     * <p>
     * Note that this method does not check whether the resource actually exists. It only constructs the path. Null inputs are not allowed.
     * </p>
     *
     * <p>
     * If the context is an unnamed package, returns {@code resourceName} unchanged.
     * </p>
     *
     * <pre>
     * ClassPathUtils.toFullyQualifiedPath(StringUtils.class.getPackage(), "StringUtils.properties") = "org/apache/commons/lang3/StringUtils.properties"
     * </pre>
     *
     * @param context      The context for constructing the path.
     * @param resourceName The resource name to construct the fully qualified path for.
     * @return The fully qualified path of the resource with name {@code resourceName}.
     * @throws NullPointerException Thrown if either {@code context} or {@code resourceName} is null.
     */
    public static String toFullyQualifiedPath(final Package context, final String resourceName) {
        Objects.requireNonNull(context, "context");
        Objects.requireNonNull(resourceName, "resourceName");
        return qualify(packageToPath(context.getName()), resourceName, '/');
    }

    /**
     * {@link ClassPathUtils} instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code ClassPathUtils.toFullyQualifiedName(MyClass.class, "MyClass.properties");}.
     *
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     *
     * @deprecated TODO Make private in 4.0.
     */
    @Deprecated
    public ClassPathUtils() {
        // empty
    }

}
