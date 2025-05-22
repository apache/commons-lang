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

import java.lang.reflect.Field;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.OffsetTime;
import java.time.Period;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Package-private reflection code.
 */
final class Reflection {

    /**
     * Some classes known to not be introspectable.
     * Those are classes where we would blow up when we try to access internal information via reflection.
     */
    private static final Set<Class> KNOWN_NON_INTROSPECTIBLE_CLASSES = new HashSet<>();
    {
        KNOWN_NON_INTROSPECTIBLE_CLASSES.addAll(Arrays.asList(
                Boolean.class,
                Character.class,
                String.class,
                Byte.class,
                Short.class,
                Integer.class,
                Long.class,
                Float.class,
                Double.class,
                java.util.Date.class,
                java.sql.Date.class,
                LocalDate.class,
                LocalDateTime.class,
                LocalTime.class,
                OffsetTime.class,
                OffsetDateTime.class,
                Instant.class,
                Duration.class,
                Period.class
                ));
    }

    /**
     * this is a cache for {@link #isNonIntrospectibleClass(Class)}.
     * Downside: this has a Class as key, so it *might* lock a ClassLoader.
     * TODO think about making this cache optional? Otoh we only cache classes we touch via the reflection methods.
     */
    private static Map<Class, Boolean> nonIntrospectibleClasses = new ConcurrentHashMap<>();

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
    static boolean isNonIntrospectibleClass(Object o) {
        return o != null && isNonIntrospectibleClass(o.getClass());
    }

    /**
     * Check whether the given class internas can be accessed. This might return false in Java9
     * and thus lead to a difference in behaviour if modularity is activated or not.
     * But it's the best we can do.
     *
     * @return {@code true} if the given class internas not accessible
     */
    static boolean isNonIntrospectibleClass(Class<?> clazz) {
        if (KNOWN_NON_INTROSPECTIBLE_CLASSES.contains(clazz)) {
            return true;
        }
        Boolean isAccessible = nonIntrospectibleClasses.get(clazz);
        if (isAccessible != null) {
            return isAccessible;
        }
        try {
            final Field[] declaredFields = clazz.getDeclaredFields();
            for (Field f : declaredFields) {
                f.setAccessible(true);
            }

            nonIntrospectibleClasses.put(clazz, false);
            return false;
        } catch (Exception e) {
            nonIntrospectibleClasses.put(clazz, true);
            return true;
        }
    }

}
