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
 * Color enumeration which demonstrates how to define the constants in a
 * different class to the Enum. The extra <code>static{}</code> block is
 * needed to ensure that the enum constants are created before the
 * static methods on the ColorEnum are used.
 * <p>
 * The class loader sees the two classes here as independent - the enum
 * class is nested, not an inner class. The static block thus forces the
 * class load of the outer class, which is needed to initialise the enums.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */

public final class NestLinked {
    
    public static final ColorEnum RED = new ColorEnum("Red");
    public static final ColorEnum GREEN = new ColorEnum("Green");
    public static final ColorEnum BLUE = new ColorEnum("Blue");
    
    public NestLinked() {
        super();
    }
    
    public static final class ColorEnum extends Enum {

        static {
            // Explicitly reference the class where the enums are defined
            Object obj = NestLinked.RED;
        }
        
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
