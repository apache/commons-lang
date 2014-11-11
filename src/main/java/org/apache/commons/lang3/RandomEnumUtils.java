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
package org.apache.commons.lang3;

import java.util.List;

/**
 * Utility library to provide a means to operate with random enum
 * instances of a certain type.
 *
 * @since 3.4
 * @version $Id$
 */
public class RandomEnumUtils {

    /**
     * Returns a random instance of an Enum type
     *
     * @param <E> the type of the enumeration
     * @param enumClass  the class of the enum to query, not null
     * @return random instance of an Enum type
     */
    public static <E extends Enum<E>> E randomEnum(final Class<E> enumClass) {
        List<E> enumList = EnumUtils.getEnumList(enumClass);
        return enumList.get(RandomUtils.nextInt(0, enumList.size()));
    }
}
