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

import java.lang.reflect.Field;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Package-private reflection code.
 */
final class Reflection {

    private static Map<Class, Boolean> inaccessibleClasses = new ConcurrentHashMap<>();

    /**
     * Delegates to {@link Field#get(Object)} and rethrows {@link IllegalAccessException} as {@link IllegalArgumentException}.
     *
     * @param field The receiver of the get call.
     * @param obj   The argument of the get call.
     * @return The result of the get call.
     * @throws IllegalArgumentException Thrown after catching {@link IllegalAccessException}.
     */
    static Object getUnchecked(final Field field, final Object obj) {
        try {
            return Objects.requireNonNull(field, "field").get(obj);
        } catch (final IllegalAccessException e) {
            throw new IllegalArgumentException(e);
        }
    }

    /**
     * Check whether the class internas of the given object can be accessed. This might return false in Java9
     * and thus lead to a difference in behaviour if modularity is activated or not.
     * But it's the best we can do.
     *
     * @return {@code true} if the given class internas not accessible
     */
    static boolean isInaccessibleClass(Object o) {
        return o != null && isInaccessibleClass(o.getClass());
    }

    /**
     * Check whether the given class internas can be accessed. This might return false in Java9
     * and thus lead to a difference in behaviour if modularity is activated or not.
     * But it's the best we can do.
     *
     * @return {@code true} if the given class internas not accessible
     */
    static boolean isInaccessibleClass(Class<?> clazz) {
        Boolean isAccessible = inaccessibleClasses.get(clazz);
        if (isAccessible != null) {
            return isAccessible;
        }
        try {
            final Field[] declaredFields = clazz.getDeclaredFields();
            for (Field f : declaredFields) {
                f.setAccessible(true);
            }

            inaccessibleClasses.put(clazz, false);
            return false;
        } catch (Exception e) {
            inaccessibleClasses.put(clazz, true);
            return true;
        }
    }

}
