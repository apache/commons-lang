/*
 * Copyright 2003,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.lang.enum;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Extended enumeration.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */
public class Extended2Enum extends Extended1Enum {
    public static final Extended1Enum GAMMA = new Extended2Enum("Gamma");

    protected Extended2Enum(String color) {
        super(color);
    }

    public static Extended1Enum getEnum(String name) {
        return (Extended1Enum) getEnum(Extended2Enum.class, name);
    }

    public static Map getEnumMap() {
        return getEnumMap(Extended2Enum.class);
    }

    public static List getEnumList() {
        return getEnumList(Extended2Enum.class);
    }

    public static Iterator iterator() {
        return iterator(Extended2Enum.class);
    }

}
