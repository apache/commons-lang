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
 * Base extended enumeration.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */
public class Extended1Enum extends Enum {
    public static final Extended1Enum ALPHA = new Extended1Enum("Alpha");
    public static final Extended1Enum BETA = new Extended1Enum("Beta");

    protected Extended1Enum(String name) {
        super(name);
    }

    public static Extended1Enum getEnum(String name) {
        return (Extended1Enum) getEnum(Extended1Enum.class, name);
    }

    public static Map getEnumMap() {
        return getEnumMap(Extended1Enum.class);
    }

    public static List getEnumList() {
        return getEnumList(Extended1Enum.class);
    }

    public static Iterator iterator() {
        return iterator(Extended1Enum.class);
    }

}
