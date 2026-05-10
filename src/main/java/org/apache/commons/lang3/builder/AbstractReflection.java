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
import java.util.function.Supplier;

import org.apache.commons.lang3.SystemProperties;

/**
 * Abstracts reflection access for reflection-based classes in this package.
 * <p>
 * See {@link AbstractBuilder#setForceAccessible(boolean)} for details.
 * </p>
 *
 * @since 3.21.0
 * @see AbstractBuilder#setForceAccessible(boolean)
 * @see AccessibleObject#setAccessible(boolean)
 */
public abstract class AbstractReflection {

    /**
     * Builds an instance of a subclass of {@link AbstractReflection}.
     *
     * @param <B> An AbstractBuilder subclass.
     */
    public abstract static class AbstractBuilder<B extends AbstractBuilder<B>> implements Supplier<AbstractReflection> {

        /**
         * Whether the {@link AbstractReflection} subclass will call {@link AccessibleObject#setAccessible(boolean) AccessibleObject#setAccessible(true)} on
         * inaccessible fields.
         */
        private boolean forceAccessible = getForceAccessible();

        /**
         * Constructs a new instance for a subclass.
         */
        AbstractBuilder() {
            // Empty.
        }

        /**
         * Returns {@code this} instance typed as its subclass.
         *
         * @return {@code this} instance typed as its subclass.
         */
        @SuppressWarnings("unchecked")
        protected B asThis() {
            return (B) this;
        }

        /**
         * Whether the {@link AbstractReflection} subclass will call {@link AccessibleObject#setAccessible(boolean) AccessibleObject#setAccessible(true)} on
         * inaccessible fields.
         * <p>
         * In general, controls whether the instances built by this builder will force the accessible flag for reflection.
         * </p>
         * <p>
         * Defaults to {@code getForceAccessible()}, which defaults to true for compatibility.
         * </p>
         * <p>
         * This default is read from the system property {@code "AbstractReflection.forceAccessible"}, which defaults to true for compatibility.
         * </p>
         * <p>
         * The parsing rules are as {@link Boolean#parseBoolean(String)}.
         * </p>
         * <p>
         * See subclassses for specific behavior.
         * </p>
         *
         * @param forceAccessible Whether to force accessibility by calling {@link AccessibleObject#setAccessible(boolean)
         *                        AccessibleObject#setAccessible(true)}.
         * @return {@code this} instance.
         * @see AccessibleObject#setAccessible(boolean)
         */
        public B setForceAccessible(final boolean forceAccessible) {
            this.forceAccessible = forceAccessible;
            return asThis();
        }
    }

    /**
     * Tests whether the system property {@code "AbstractReflection.forceAccessible"} is set to true.
     * <p>
     * The parsing rules are as {@link Boolean#parseBoolean(String)}.
     * </p>
     * <p>
     * If the property is not set, return true.
     * </p>
     *
     * @return whether the system property {@code "AbstractReflection.forceAccessible"} is set to true with true as the default.
     * @see Boolean#parseBoolean(String)
     */
    static boolean getForceAccessible() {
        return SystemProperties.getBoolean(AbstractReflection.class, "forceAccessible", () -> true);
    }

    /**
     * If {@code forceAccessible} flag is true, each field in the given array is made accessible by calling {@link AccessibleObject#setAccessible(boolean)
     * AccessibleObject#setAccessible(true)} but <em>only</em> if a field is not already accessible.
     *
     * @param forceAccessible Whether to call {@link AccessibleObject#setAccessible(boolean)} if a field is not already accessible.
     * @param fields          The fields to set.
     * @throws SecurityException Thrown if {@code forceAccessible} flag is true and the request is denied.
     * @see AccessibleObject#setAccessible(boolean)
     * @see SecurityManager#checkPermission
     */
    static void setAccessible(final boolean forceAccessible, final Field... fields) {
        if (forceAccessible) {
            for (final Field field : fields) {
                // Test to avoid the permission check if there is a security manager.
                if (field != null && !field.isAccessible()) {
                    field.setAccessible(true);
                }
            }
        }
    }

    /**
     * Whether to call {@link AccessibleObject#setAccessible(boolean) AccessibleObject#setAccessible(true)} on inaccessible fields.
     */
    private final boolean forceAccessible;

    /**
     * Constructs a new instance.
     *
     * @param <T>     The type to build.
     * @param builder The builder.
     */
    <T extends AbstractBuilder<T>> AbstractReflection(final AbstractBuilder<T> builder) {
        this.forceAccessible = builder.forceAccessible;
    }

    /**
     * Tests whether fields should be made accessible with {@link AccessibleObject#setAccessible(boolean)}.
     *
     * @return whether fields should be made accessible with {@link AccessibleObject#setAccessible(boolean)}.
     */
    protected boolean isForceAccessible() {
        return forceAccessible;
    }

    /**
     * If {@code forceAccessible} flag is true, each field in the given array is made accessible by calling {@link AccessibleObject#setAccessible(boolean)
     * AccessibleObject#setAccessible(true)} but <em>only</em> if a field is not already accessible.
     *
     * @param fields The fields to set.
     * @throws SecurityException Thrown if {@code forceAccessible} flag is true and the request is denied.
     * @see AccessibleObject#setAccessible(boolean)
     * @see SecurityManager#checkPermission
     */
    void setAccessible(final Field... fields) {
        setAccessible(isForceAccessible(), fields);
    }
}
