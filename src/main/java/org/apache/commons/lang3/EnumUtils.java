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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Utility library to provide helper methods for Java enums.
 * 
 * <p>#ThreadSafe#</p>
 * @author Apache Software Foundation
 */
public class EnumUtils {

    /**
     * This constructor is public to permit tools that require a JavaBean
     * instance to operate.
     */
    public EnumUtils() {
    }

    /**
     * Gets the <code>Map</code> of <code>enums</code> by name.
     * <p>
     * This method is useful when you need a map of enums by name.
     *
     * @param enumClass  the class of the <code>enum</code> to get, not null
     * @return the modifiable map of enum names to enums, never null
     */
    public static <E extends Enum<E>> Map<String, E> getEnumMap(Class<E> enumClass) {
        Map<String, E> map = new LinkedHashMap<String, E>();
        for (E e: enumClass.getEnumConstants()) {
            map.put(e.name(), e);
        }
        return map;
    }

    /**
     * Gets the <code>List</code> of <code>enums</code>.
     * <p>
     * This method is useful when you need a list of enums rather than an array.
     *
     * @param enumClass  the class of the <code>enum</code> to get, not null
     * @return the modifiable list of enums, never null
     */
    public static <E extends Enum<E>> List<E> getEnumList(Class<E> enumClass) {
        return new ArrayList<E>(Arrays.asList(enumClass.getEnumConstants()));
    }

    /**
     * Checks if the specified name is a valid <code>enum</code> for the class.
     * <p>
     * This method differs from {@link Enum#valueOf} in that checks if the name is
     * a valid enum without needing to catch the exception.
     *
     * @param enumClass  the class of the <code>enum</code> to get, not null
     * @param enumName   the enum name
     * @return true if the enum name is valid, otherwise false
     */
    public static <E extends Enum<E>> boolean isValidEnum(Class<E> enumClass, String enumName) {
        try {
            Enum.valueOf(enumClass, enumName);
            return true;
        } catch (IllegalArgumentException ex) {
            return false;
        }
    }

    /**
     * Gets the <code>enum</code> for the class, returning <code>null</code> if not found.
     * <p>
     * This method differs from {@link Enum#valueOf} in that it does not throw an exception
     * for an invalid enum name.
     *
     * @param enumClass  the class of the <code>enum</code> to get, not null
     * @param enumName   the enum name
     * @return the enum or null if not found
     */
    public static <E extends Enum<E>> E getEnum(Class<E> enumClass, String enumName) {
        try {
            return Enum.valueOf(enumClass, enumName);
        } catch (IllegalArgumentException ex) {
            return null;
        }
    }

}
