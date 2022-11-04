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
     * Converts a package name to a Java path ('/').
     *
     * @param path the source path.
     * @return a package name.
     * @since 3.13.0
     */
    public static String packageToPath(final String path) {
        return Objects.requireNonNull(path, "path").replace('.', '/');
    }

    /**
     * Converts a Java path ('/') to a package name.
     *
     * @param path the source path.
     * @return a package name.
     * @since 3.13.0
     */
    public static String pathToPackage(final String path) {
        return Objects.requireNonNull(path, "path").replace('/', '.');
    }

    /**
     * Returns the fully qualified name for the resource with name {@code resourceName} relative to the given context.
     *
     * <p>
     * Note that this method does not check whether the resource actually exists. It only constructs the name. Null inputs are not allowed.
     * </p>
     *
     * <pre>
     * ClassPathUtils.toFullyQualifiedName(StringUtils.class, "StringUtils.properties") = "org.apache.commons.lang3.StringUtils.properties"
     * </pre>
     *
     * @param context      The context for constructing the name.
     * @param resourceName the resource name to construct the fully qualified name for.
     * @return the fully qualified name of the resource with name {@code resourceName}.
     * @throws NullPointerException if either {@code context} or {@code resourceName} is null.
     */
    public static String toFullyQualifiedName(final Class<?> context, final String resourceName) {
        Objects.requireNonNull(context, "context");
        Objects.requireNonNull(resourceName, "resourceName");
        return toFullyQualifiedName(context.getPackage(), resourceName);
    }

    /**
     * Returns the fully qualified name for the resource with name {@code resourceName} relative to the given context.
     *
     * <p>
     * Note that this method does not check whether the resource actually exists. It only constructs the name. Null inputs are not allowed.
     * </p>
     *
     * <pre>
     * ClassPathUtils.toFullyQualifiedName(StringUtils.class.getPackage(), "StringUtils.properties") = "org.apache.commons.lang3.StringUtils.properties"
     * </pre>
     *
     * @param context      The context for constructing the name.
     * @param resourceName the resource name to construct the fully qualified name for.
     * @return the fully qualified name of the resource with name {@code resourceName}.
     * @throws NullPointerException if either {@code context} or {@code resourceName} is null.
     */
    public static String toFullyQualifiedName(final Package context, final String resourceName) {
        Objects.requireNonNull(context, "context");
        Objects.requireNonNull(resourceName, "resourceName");
        return context.getName() + "." + resourceName;
    }

    /**
     * Returns the fully qualified path for the resource with name {@code resourceName} relative to the given context.
     *
     * <p>
     * Note that this method does not check whether the resource actually exists. It only constructs the path. Null inputs are not allowed.
     * </p>
     *
     * <pre>
     * ClassPathUtils.toFullyQualifiedPath(StringUtils.class, "StringUtils.properties") = "org/apache/commons/lang3/StringUtils.properties"
     * </pre>
     *
     * @param context      The context for constructing the path.
     * @param resourceName the resource name to construct the fully qualified path for.
     * @return the fully qualified path of the resource with name {@code resourceName}.
     * @throws NullPointerException if either {@code context} or {@code resourceName} is null.
     */
    public static String toFullyQualifiedPath(final Class<?> context, final String resourceName) {
        Objects.requireNonNull(context, "context");
        Objects.requireNonNull(resourceName, "resourceName");
        return toFullyQualifiedPath(context.getPackage(), resourceName);
    }

    /**
     * Returns the fully qualified path for the resource with name {@code resourceName} relative to the given context.
     *
     * <p>
     * Note that this method does not check whether the resource actually exists. It only constructs the path. Null inputs are not allowed.
     * </p>
     *
     * <pre>
     * ClassPathUtils.toFullyQualifiedPath(StringUtils.class.getPackage(), "StringUtils.properties") = "org/apache/commons/lang3/StringUtils.properties"
     * </pre>
     *
     * @param context      The context for constructing the path.
     * @param resourceName the resource name to construct the fully qualified path for.
     * @return the fully qualified path of the resource with name {@code resourceName}.
     * @throws NullPointerException if either {@code context} or {@code resourceName} is null.
     */
    public static String toFullyQualifiedPath(final Package context, final String resourceName) {
        Objects.requireNonNull(context, "context");
        Objects.requireNonNull(resourceName, "resourceName");
        return packageToPath(context.getName()) + "/" + resourceName;
    }

    /**
     * {@link ClassPathUtils} instances should NOT be constructed in standard programming. Instead, the class should be used as
     * {@code ClassPathUtils.toFullyQualifiedName(MyClass.class, "MyClass.properties");}.
     *
     * <p>
     * This constructor is public to permit tools that require a JavaBean instance to operate.
     * </p>
     */
    public ClassPathUtils() {
    }

}
