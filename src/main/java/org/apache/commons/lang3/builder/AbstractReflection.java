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
 * Abstracts reflection access for reflection classes in this package.
 *
 * @since 3.21.0
 */
public abstract class AbstractReflection {

    /**
     * Builds a subclass.
     *
     * @param <B> This Builder type.
     */
    public abstract static class AbstractBuilder<B extends AbstractBuilder<B>> implements Supplier<AbstractReflection> {

        /**
         * Whether to set the the {@code accessible} flag.
         */
        private boolean accessibleFlag = accessibleFlag();

        AbstractBuilder() {
            // Empty.
        }

        /**
         * Returns {@code this} instance typed as a subclass.
         *
         * @return {@code this} instance typed as a subclass.
         */
        @SuppressWarnings("unchecked")
        protected B asThis() {
            return (B) this;
        }

        /**
         * Sets the forceAccessible flag, defaults to {@code forceAccessible()} which defaults to true.
         * <p>
         * In general, controls whether the instances built by this builder will force the accessible flag for reflection.
         * </p>
         * <p>
         * See subclassses for specific behavior.
         * </p>
         *
         * @param forceAccessible Whether to force accessibility by calling {@link AccessibleObject#setAccessible(boolean)}.
         * @return {@code this} instance.
         */
        public B setForceAccessible(final boolean forceAccessible) {
            this.accessibleFlag = forceAccessible;
            return asThis();
        }
    }

    /**
     * Tests whether the system property {@code "AbstractReflection.forceAccessible"} is set to true.
     *
     * <p>
     * If the property is not set, return true.
     * </p>
     *
     * @return whether the system property {@code "AbstractReflection.forceAccessible"} is set to true with true as the default.
     */
    static boolean accessibleFlag() {
        return SystemProperties.getBoolean(AbstractReflection.class, "forceAccessible", () -> true);
    }

    /**
     * If {@code forceAccessible} flag is true, each field in the given array is made accessible via {@link AccessibleObject#setAccessible(boolean)} only if a
     * field is not already accessible.
     *
     * @param accessibleFlag Whether to call {@link AccessibleObject#setAccessible(boolean)} if a field is not already accessible.
     * @param fields         The fields to set.
     * @throws SecurityException Thrown if {@code forceAccessible} flag is true and the request is denied.
     * @see SecurityManager#checkPermission
     * @see RuntimePermission
     */
    static void setAccessible(final boolean accessibleFlag, final Field[] fields) {
        if (accessibleFlag) {
            for (final Field field : fields) {
                // Test to avoid the permission check if there is a security manager.
                if (!field.isAccessible()) {
                    field.setAccessible(true);
                }
            }
        }
    }

    /**
     * Whether to set the the {@code accessible} flag.
     */
    private final boolean accessibleFlag;


    /**
     * Constructs a new instance.
     *
     * @param <T> The type to build.
     * @param builder The builder.
     */
    <T extends AbstractBuilder<T>> AbstractReflection(final AbstractBuilder<T> builder) {
        this.accessibleFlag = builder.accessibleFlag;
    }

    /**
     * Tests whether fields should be made accessible.
     *
     * @return whether fields should be made accessible.
     */
    protected boolean isAccessible() {
        return accessibleFlag;
    }

    void setAccessible(final Field[] fields) {
        setAccessible(accessibleFlag, fields);
    }
}
