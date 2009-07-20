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
package org.apache.commons.lang;

import java.util.Map;
import java.util.LinkedHashMap;
import java.util.EnumSet;

/**
 * Utility library to provide helper methods for Java enums. 
 */
public class EnumUtils {
    
    /**
     * <p>This constructor is public to permit tools that require a JavaBean
     * instance to operate.</p>
     */
    public EnumUtils() {
    }

    /**
     * <p>Gets the <code>Map</code> of <code>enums</code> by name.</p>
     *
     * @param enumClass the class of the <code>enum</code> to get
     * @return the enum Map
     */
    public static <E extends Enum<E>> Map<String, Enum<E>> getEnumMap(Class<E> enumClass) {
        Map<String, Enum<E>> map = new LinkedHashMap<String, Enum<E>>();

        for (E e: EnumSet.allOf(enumClass)) {
            map.put(e.name(), e);
        }

        return map;
    }
    
}
