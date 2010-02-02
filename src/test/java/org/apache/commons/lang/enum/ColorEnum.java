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
package org.apache.commons.lang.enum;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Color enumeration.
 *
 * @author Apache Software Foundation
 * @version $Id$
 */

public final class ColorEnum extends Enum {
    public static final ColorEnum RED = new ColorEnum("Red");
    public static final ColorEnum GREEN = new ColorEnum("Green");
    public static final ColorEnum BLUE = new ColorEnum("Blue");

    private ColorEnum(String color) {
        super(color);
    }

    public static ColorEnum getEnum(String color) {
        return (ColorEnum) getEnum(ColorEnum.class, color);
    }

    public static Map getEnumMap() {
        return getEnumMap(ColorEnum.class);
    }

    public static List getEnumList() {
        return getEnumList(ColorEnum.class);
    }

    public static Iterator iterator() {
        return iterator(ColorEnum.class);
    }
}
