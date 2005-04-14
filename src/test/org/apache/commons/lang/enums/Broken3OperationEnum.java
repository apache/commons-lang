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
 * Broken Operator enumeration, getEnumClass() is Enum.class.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */
public abstract class Broken3OperationEnum extends Enum {
    // This syntax works for JDK 1.3 and upwards:
//    public static final OperationEnum PLUS = new OperationEnum("Plus") {
//        public int eval(int a, int b) {
//            return (a + b);
//        }
//    };
//    public static final OperationEnum MINUS = new OperationEnum("Minus") {
//        public int eval(int a, int b) {
//            return (a - b);
//        }
//    };
    // This syntax works for JDK 1.2 and upwards:
    public static final Broken3OperationEnum PLUS = new PlusOperation();
    private static class PlusOperation extends Broken3OperationEnum {
        private PlusOperation() {
            super("Plus");
        }
        public int eval(int a, int b) {
            return (a + b);
        }
    }
    public static final Broken3OperationEnum MINUS = new MinusOperation();
    private static class MinusOperation extends Broken3OperationEnum {
        private MinusOperation() {
            super("Minus");
        }
        public int eval(int a, int b) {
            return (a - b);
        }
    }

    private Broken3OperationEnum(String name) {
        super(name);
    }
    
    public final Class getEnumClass() {
        return Enum.class;
    }

    public abstract int eval(int a, int b);

    public static Broken3OperationEnum getEnum(String name) {
        return (Broken3OperationEnum) getEnum(Broken3OperationEnum.class, name);
    }

    public static Map getEnumMap() {
        return getEnumMap(Broken3OperationEnum.class);
    }

    public static List getEnumList() {
        return getEnumList(Broken3OperationEnum.class);
    }

    public static Iterator iterator() {
        return iterator(Broken3OperationEnum.class);
    }
}
