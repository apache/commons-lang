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
package org.apache.commons.lang.enums;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Language enumeration.
 *
 * @author Apache Software Foundation
 * @version $Id$
 */
public final class ValuedLanguageEnum extends ValuedEnum {
    public static final ValuedLanguageEnum ENGLISH = new ValuedLanguageEnum("English", 1);
    public static final ValuedLanguageEnum FRENCH = new ValuedLanguageEnum("French", 2);
    public static final ValuedLanguageEnum GERMAN = new ValuedLanguageEnum("German", 3);

    private ValuedLanguageEnum(String color, int value) {
        super(color, value);
    }

    public static ValuedLanguageEnum getEnum(String color) {
        return (ValuedLanguageEnum) getEnum(ValuedLanguageEnum.class, color);
    }

    public static ValuedLanguageEnum getEnum(int value) {
        return (ValuedLanguageEnum) getEnum(ValuedLanguageEnum.class, value);
    }

    public static Map getEnumMap() {
        return getEnumMap(ValuedLanguageEnum.class);
    }

    public static List getEnumList() {
        return getEnumList(ValuedLanguageEnum.class);
    }

    public static Iterator iterator() {
        return iterator(ValuedLanguageEnum.class);
    }
}
