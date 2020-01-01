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

package org.apache.commons.lang3.compare;

import java.io.Serializable;
import java.util.Comparator;

/**
 * Compares Object's {@link Object#toString()} values.
 *
 * This class is stateless.
 *
 * @since 3.10
 */
public final class ObjectToStringComparator implements Comparator<Object>, Serializable {

    /**
     * Singleton instance.
     *
     * This class is stateless.
     */
    public static final ObjectToStringComparator INSTANCE = new ObjectToStringComparator();

    /**
     * For {@link Serializable}.
     */
    private static final long serialVersionUID = 1L;

    @Override
    public int compare(final Object o1, final Object o2) {
        if (o1 == null && o2 == null) {
            return 0;
        }
        if (o1 == null) {
            return 1;
        }
        if (o2 == null) {
            return -1;
        }
        final String string1 = o1.toString();
        final String string2 = o2.toString();
        // No guarantee that toString() returns a non-null value, despite what Spotbugs thinks.
        if (string1 == null && string2 == null) {
            return 0;
        }
        if (string1 == null) {
            return 1;
        }
        if (string2 == null) {
            return -1;
        }
        return string1.compareTo(string2);
    }
}
