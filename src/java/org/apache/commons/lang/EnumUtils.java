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

import java.util.Iterator;
import java.util.Map;
import java.util.LinkedHashMap;
import java.util.EnumSet;

/**
 * Utility library to provide helper methods for Java enums. 
 */
public class EnumUtils {
    
    /**
     * Constructor. This class should not normally be instantiated.
     */
    public EnumUtils() {
    }

    /**
     * <p>Gets the <code>Map</code> of <code>enums</code> by name.</p>
     *
     * @param enumClass the class of the <code>enum</code> to get
     * @return the enum Map
     */
    public static Map getEnumMap(Class enumClass) {
        Map map = new LinkedHashMap();
        Iterator itr = EnumSet.allOf(enumClass).iterator();
        while(itr.hasNext()) { Enum enm = (Enum) itr.next(); map.put( enm.name(), enm ); }
        return map;
    }
    
}
