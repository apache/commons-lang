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
import java.time.temporal.Temporal;
import java.util.Objects;

/**
 * Package-private reflection code.
 */
final class Reflection {

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
     * Check whether the given object is of a type which is an internal java Class, like String, Integer, Boolean, LocalDateTime, etc
     * This is often needed as we cannot look into them via reflection since Java9.
     *
     * @return {@code true} if the given object is a Java internal class
     */
    static boolean isJavaInternalClass(Object o) {
        return o != null && isJavaInternalClass(o.getClass());
    }

    /**
     * Check whether the given class is an internal java Class, like String, Integer, Boolean, LocalDateTime, etc
     * This is often needed as we cannot look into them via reflection since Java9.
     *
     * @return {@code true} if the given class is a Java internal class
     */
    static boolean isJavaInternalClass(Class<?> clazz) {
        return CharSequence.class.isAssignableFrom(clazz) || Number.class.isAssignableFrom(clazz) || Boolean.class.isAssignableFrom(clazz) || Temporal.class.isAssignableFrom(clazz);
    }

}
