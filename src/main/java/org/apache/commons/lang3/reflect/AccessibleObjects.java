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

import java.lang.reflect.AccessibleObject;

/**
 * Null-safe operations on {@link AccessibleObject}.
 */
class AccessibleObjects {

    /**
     * Delegates to {@link AccessibleObject#isAccessible()} if {@code accessibleObject} isn't null.
     *
     * @param accessibleObject The accessible object.
     * @return The value of the object's {@code accessible} flag
     */
    static boolean isAccessible(final AccessibleObject accessibleObject) {
        return accessibleObject == null || accessibleObject.isAccessible();
    }

    /**
     * Delegates to {@link AccessibleObject#setAccessible(boolean)} only if {@link AccessibleObject#isAccessible()} returns false. This avoid a
     * permission check if there is a security manager.
     *
     * @param accessibleObject The accessible object.
     * @return Whether {@link AccessibleObject#setAccessible(boolean)} was called.
     */
    static boolean setAccessible(final AccessibleObject accessibleObject) {
        if (!isAccessible(accessibleObject)) {
            accessibleObject.setAccessible(true);
            return true;
        }
        return false;
    }
}
