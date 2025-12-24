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
package org.apache.commons.lang3.builder;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * Utility class to determine whether reflective access to a target module is permitted from the current module.
 * This class assumes all reflective accesses are allowed in JVM where JPMS is not supported (i.e. JDK8 or earlier).
 */
class ReflectiveAccessUtil {

    /**
     * Determines whether {@code targetClass} is accessible via reflection from the module that defines this class.
     * On JVMs that do not support the Java Platform Module System, the method always returns {@code true}.
     * Otherwise, it checks whether the module that declares {@code targetClass} is open to the module containing
     * this class at runtime.
     *
     * @param targetClass the {@link Class} object to inspect.
     * @return {@code true} if {@code targetClass} can be reflected on from this module,
     *         {@code false} otherwise.
     */
    boolean isAllowed(Class<?> targetClass) {
        if (!isJpmsSupported()) {
            return true;
        }
        if (targetClass.isArray()) {
            return true;
        }
        try {
            final Object targetModule = getModule.invoke(targetClass);
            return (Boolean) isOpen.invoke(targetModule, targetClass.getPackage().getName(), selfModule);
        } catch (IllegalAccessException | InvocationTargetException e) {
            return false;
        }
    }

    /**
     * Indicates whether the current JVM supports the Java Platform Module System (JPMS).
     * @return {@code true} if JPMS is available.
     */
    boolean isJpmsSupported() {
        return getModule != null;
    }

    // Cache the following methods and object in static fields to minimize performance impact.
    private static final Method getModule = initializeGetModuleMethod();
    private static final Object selfModule = initializeSelfModule();
    private static final Method isOpen = initializeIsOpenMethod();

    private static Method initializeGetModuleMethod() {
        try {
            return Class.class.getMethod("getModule");
        } catch (NoSuchMethodException e) {
            return null;
        }
    }

    private static Object initializeSelfModule() {
        if (getModule == null) {
            return null;
        }
        try {
            return getModule.invoke(ReflectiveAccessUtil.class);
        } catch (InvocationTargetException | IllegalAccessException e) {
            return null;
        }
    }

    private static Method initializeIsOpenMethod() {
        if (selfModule == null) {
            return null;
        }
        try {
            final Class<?> moduleClass = selfModule.getClass();
            return moduleClass.getMethod("isOpen", String.class, moduleClass);
        } catch (NoSuchMethodException e) {
            return null;
        }
    }
}
