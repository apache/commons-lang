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
package org.apache.commons.lang.enum;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Broken Operator enumeration, getEnumClass() not superclass.
 *
 * @author Stephen Colebourne
 * @version $Id$
 */
public abstract class Broken2OperationEnum extends Enum {
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
    public static final Broken2OperationEnum PLUS = new PlusOperation();
    private static class PlusOperation extends Broken2OperationEnum {
        private PlusOperation() {
            super("Plus");
        }
        public int eval(int a, int b) {
            return (a + b);
        }
    }
    public static final Broken2OperationEnum MINUS = new MinusOperation();
    private static class MinusOperation extends Broken2OperationEnum {
        private MinusOperation() {
            super("Minus");
        }
        public int eval(int a, int b) {
            return (a - b);
        }
    }

    private Broken2OperationEnum(String name) {
        super(name);
    }
    
    public final Class getEnumClass() {
        return ColorEnum.class;
    }

    public abstract int eval(int a, int b);

    public static Broken2OperationEnum getEnum(String name) {
        return (Broken2OperationEnum) getEnum(Broken2OperationEnum.class, name);
    }

    public static Map getEnumMap() {
        return getEnumMap(Broken2OperationEnum.class);
    }

    public static List getEnumList() {
        return getEnumList(Broken2OperationEnum.class);
    }

    public static Iterator iterator() {
        return iterator(Broken2OperationEnum.class);
    }
}
