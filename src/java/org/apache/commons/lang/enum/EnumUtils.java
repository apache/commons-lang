/* ====================================================================
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002-2003 The Apache Software Foundation.  All rights
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

import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * <p>Utility class for accessing and manipulating {@link Enum}s.</p>
 *
 * @see Enum
 * @see ValuedEnum
 * @author Stephen Colebourne
 * @since 1.0
 * @version $Id: EnumUtils.java,v 1.8 2003/07/30 23:21:39 scolebourne Exp $
 */
public class EnumUtils {

    /**
     * Public constructor. This class should not normally be instantiated.
     */
    public EnumUtils() {
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
