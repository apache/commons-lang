/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Commons", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
package org.apache.commons.lang.enum;

import java.io.Serializable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
/**
 * Utility class for accessing and manipulating Enums.
 *
 * @see Enum
 * @see ValuedEnum
 * @author Stephen Colebourne
 * @since 1.0
 * @version $Id: EnumUtils.java,v 1.4 2002/12/23 00:17:06 scolebourne Exp $
 */
public abstract class EnumUtils implements Comparable, Serializable {

    /**
     * Restricted constructor
     */
    private EnumUtils() {
    }

    /**
     * Gets an Enum object by class and name.
     * 
     * @param enumClass  the class of the Enum to get
     * @param name  the name of the Enum to get, may be null
     * @return the enum object
     * @throws IllegalArgumentException if the enum class is null
     */
    public static Enum getEnum(Class enumClass, String name) {
        return Enum.getEnum(enumClass, name);
    }

    /**
     * Gets a ValuedEnum object by class and value.
     * 
     * @param enumClass  the class of the Enum to get
     * @param value  the value of the Enum to get
     * @return the enum object, or null if the enum does not exist
     * @throws IllegalArgumentException if the enum class is null
     */
    public static ValuedEnum getEnum(Class enumClass, int value) {
        return (ValuedEnum) ValuedEnum.getEnum(enumClass, value);
    }

    /**
     * Gets the Map of Enum objects by name using the Enum class.
     * If the requested class has no enum objects an empty Map is returned.
     * The Map is unmodifiable.
     * 
     * @param enumClass  the class of the Enum to get
     * @return the enum object Map
     * @throws IllegalArgumentException if the enum class is null
     * @throws IllegalArgumentException if the enum class is not a subclass of Enum
     */
    public static Map getEnumMap(Class enumClass) {
        return Enum.getEnumMap(enumClass);
    }

    /**
     * Gets the List of Enum objects using the Enum class.
     * The list is in the order that the objects were created (source code order).
     * If the requested class has no enum objects an empty List is returned.
     * The List is unmodifiable.
     * 
     * @param enumClass  the class of the Enum to get
     * @return the enum object Map
     * @throws IllegalArgumentException if the enum class is null
     * @throws IllegalArgumentException if the enum class is not a subclass of Enum
     */
    public static List getEnumList(Class enumClass) {
        return Enum.getEnumList(enumClass);
    }

    /**
     * Gets an iterator over the Enum objects in an Enum class.
     * The iterator is in the order that the objects were created (source code order).
     * If the requested class has no enum objects an empty Iterator is returned.
     * The Iterator is unmodifiable.
     * 
     * @param enumClass  the class of the Enum to get
     * @return an iterator of the Enum objects
     * @throws IllegalArgumentException if the enum class is null
     * @throws IllegalArgumentException if the enum class is not a subclass of Enum
     */
    public static Iterator iterator(Class enumClass) {
        return Enum.getEnumList(enumClass).iterator();
    }
    
}
