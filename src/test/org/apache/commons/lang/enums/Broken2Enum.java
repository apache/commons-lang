/*
 * Copyright 2002-2005 The Apache Software Foundation.
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
package org.apache.commons.lang.enums;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Broken color enumeration.
 *
 * @author <a href="mailto:scolebourne@joda.org">Stephen Colebourne</a>
 * @version $Id$
 */
public final class Broken2Enum extends Enum {
    public static final Broken2Enum RED = new Broken2Enum("Red");
    public static final Broken2Enum GREEN = new Broken2Enum("Green");
    public static final Broken2Enum BLUE = new Broken2Enum("");  // blank not allowed

    private Broken2Enum(String color) {
        super(color);
    }

    public static Broken2Enum getEnum(String color) {
        return (Broken2Enum) getEnum(Broken2Enum.class, color);
    }

    public static Map getEnumMap() {
        return getEnumMap(Broken2Enum.class);
    }

    public static List getEnumList() {
        return getEnumList(Broken2Enum.class);
    }

    public static Iterator iterator() {
        return iterator(Broken2Enum.class);
    }
}
