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
 * Color enumeration which demonstrates how to break the enum system.
 * <p>
 * The class loader sees the two classes here as independent - the enum
 * class is nested, not an inner class. Calling getEnumList() on ColorEnum
 * will return an empty list, unless and until the NestBroken class is
 * referenced.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */

public final class NestBroken {
    
    public static final ColorEnum RED = new ColorEnum("Red");
    public static final ColorEnum GREEN = new ColorEnum("Green");
    public static final ColorEnum BLUE = new ColorEnum("Blue");
    
    public NestBroken() {
        super();
    }
    
    public static final class ColorEnum extends Enum {

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
}
