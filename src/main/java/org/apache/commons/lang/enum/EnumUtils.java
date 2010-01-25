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
 * <p>Utility class for accessing and manipulating {@link Enum}s.</p>
 *
 * @deprecated Replaced by {@link org.apache.commons.lang.enums.EnumUtils org.apache.commons.lang.enums.EnumUtils} 
 *          and will be removed in version 3.0. All classes in this package are deprecated and repackaged to 
 *          {@link org.apache.commons.lang.enums} since <code>enum</code> is a Java 1.5 keyword. 
 * @see org.apache.commons.lang.enums.EnumUtils
 * @see Enum
 * @see ValuedEnum
 * @author Stephen Colebourne
 * @author Gary Gregory
 * @since 1.0
 * @version $Id$
 */
public class EnumUtils {

    /**
     * Public constructor. This class should not normally be instantiated.
     * @since 2.0
     */
    public EnumUtils() {
        super();
    }

    /**
     * <p>Gets an <code>Enum</code> object by class and name.</p>
     * 
     * @param enumClass  the class of the <code>Enum</code> to get
     * @param name  the name of the Enum to get, may be <code>null</code>
     * @return the enum object
     * @throws IllegalArgumentException if the enum class is <code>null</code>
     */
    public static Enum getEnum(Class enumClass, String name) {
        return Enum.getEnum(enumClass, name);
    }

    /**
     * <p>Gets a <code>ValuedEnum</code> object by class and value.</p>
     * 
     * @param enumClass  the class of the <code>Enum</code> to get
     * @param value  the value of the <code>Enum</code> to get
     * @return the enum object, or null if the enum does not exist
     * @throws IllegalArgumentException if the enum class is <code>null</code>
     */
    public static ValuedEnum getEnum(Class enumClass, int value) {
        return (ValuedEnum) ValuedEnum.getEnum(enumClass, value);
    }

    /**
     * <p>Gets the <code>Map</code> of <code>Enum</code> objects by
     * name using the <code>Enum</code> class.</p>
     *
     * <p>If the requested class has no enum objects an empty
     * <code>Map</code> is returned. The <code>Map</code> is unmodifiable.</p>
     * 
     * @param enumClass  the class of the <code>Enum</code> to get
     * @return the enum object Map
     * @throws IllegalArgumentException if the enum class is <code>null</code>
     * @throws IllegalArgumentException if the enum class is not a subclass
     *  of <code>Enum</code>
     */
    public static Map getEnumMap(Class enumClass) {
        return Enum.getEnumMap(enumClass);
    }

    /**
     * <p>Gets the <code>List</code> of <code>Enum</code> objects using
     * the <code>Enum</code> class.</p>
     *
     * <p>The list is in the order that the objects were created
     * (source code order).</p>
     *
     * <p>If the requested class has no enum objects an empty
     * <code>List</code> is returned. The <code>List</code> is unmodifiable.</p>
     * 
     * @param enumClass  the class of the Enum to get
     * @return the enum object Map
     * @throws IllegalArgumentException if the enum class is <code>null</code>
     * @throws IllegalArgumentException if the enum class is not a subclass
     *  of <code>Enum</code>
     */
    public static List getEnumList(Class enumClass) {
        return Enum.getEnumList(enumClass);
    }

    /**
     * <p>Gets an <code>Iterator</code> over the <code>Enum</code> objects
     * in an <code>Enum</code> class.</p>
     *
     * <p>The iterator is in the order that the objects were created
     * (source code order).</p>
     *
     * <p>If the requested class has no enum objects an empty
     * <code>Iterator</code> is returned. The <code>Iterator</code>
     * is unmodifiable.</p>
     * 
     * @param enumClass  the class of the <code>Enum</code> to get
     * @return an <code>Iterator</code> of the <code>Enum</code> objects
     * @throws IllegalArgumentException if the enum class is <code>null</code>
     * @throws IllegalArgumentException if the enum class is not a subclass of <code>Enum</code>
     */
    public static Iterator iterator(Class enumClass) {
        return Enum.getEnumList(enumClass).iterator();
    }
    
}
