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

import org.apache.commons.lang.ClassUtils;

/**
 * <p>Abstract superclass for type-safe enums with integer values suitable
 * for use in <code>switch</code> statements.</p>
 *
 * <p><em>NOTE:</em>Due to the way in which Java ClassLoaders work, comparing
 * <code>Enum</code> objects should always be done using the equals() method,
 * not <code>==</code>. The equals() method will try <code>==</code> first so
 * in most cases the effect is the same.</p>
 *
 * <p>To use this class, it must be subclassed. For example:</p>
 *
 * <pre>
 * public final class JavaVersionEnum extends ValuedEnum {
 *   //standard enums for version of JVM
 *   public static final int  JAVA1_0_VALUE  = 100;
 *   public static final int  JAVA1_1_VALUE  = 110;
 *   public static final int  JAVA1_2_VALUE  = 120;
 *   public static final int  JAVA1_3_VALUE  = 130;
 *   public static final JavaVersionEnum  JAVA1_0  = new JavaVersionEnum( "Java 1.0", JAVA1_0_VALUE );
 *   public static final JavaVersionEnum  JAVA1_1  = new JavaVersionEnum( "Java 1.1", JAVA1_1_VALUE );
 *   public static final JavaVersionEnum  JAVA1_2  = new JavaVersionEnum( "Java 1.2", JAVA1_2_VALUE );
 *   public static final JavaVersionEnum  JAVA1_3  = new JavaVersionEnum( "Java 1.3", JAVA1_3_VALUE );
 *
 *   private JavaVersionEnum(String name, int value) {
 *     super( name, value );
 *   }
 * 
 *   public static JavaVersionEnum getEnum(String javaVersion) {
 *     return (JavaVersionEnum) getEnum(JavaVersionEnum.class, javaVersion);
 *   }
 * 
 *   public static JavaVersionEnum getEnum(int javaVersion) {
 *     return (JavaVersionEnum) getEnum(JavaVersionEnum.class, javaVersion);
 *   }
 * 
 *   public static Map getEnumMap() {
 *     return getEnumMap(JavaVersionEnum.class);
 *   }
 * 
 *   public static List getEnumList() {
 *     return getEnumList(JavaVersionEnum.class);
 *   }
 * 
 *   public static Iterator iterator() {
 *     return iterator(JavaVersionEnum.class);
 *   }
 * }
 * </pre>
 *
 * <p>The above class could then be used as follows:</p>
 *
 * <pre>
 * public void doSomething(JavaVersionEnum ver) {
 *   switch (ver.getValue()) {
 *     case JAVA1_0_VALUE:
 *       // ...
 *       break;
 *     case JAVA1_1_VALUE:
 *       // ...
 *       break;
 *     //...
 *   }
 * }
 * </pre>
 *
 * <p>As shown, each enum has a name and a value. These can be accessed using
 * <code>getName</code> and <code>getValue</code>.</p>
 *
 * <p>The <code>getEnum</code> and <code>iterator</code> methods are recommended.
 * Unfortunately, Java restrictions require these to be coded as shown in each subclass.
 * An alternative choice is to use the {@link EnumUtils} class.</p>
 *
 * @deprecated Replaced by {@link org.apache.commons.lang.enums.ValuedEnum org.apache.commons.lang.enums.ValuedEnum}
 *          and will be removed in version 3.0. All classes in this package are deprecated and repackaged to 
 *          {@link org.apache.commons.lang.enums} since <code>enum</code> is a Java 1.5 keyword. 
 * @see org.apache.commons.lang.enums.ValuedEnum
 * @author Apache Avalon project
 * @author Stephen Colebourne
 * @since 1.0
 * @version $Id$
 */
public abstract class ValuedEnum extends Enum {
    
    /**
     * Required for serialization support. Lang version 1.0.1 serial compatibility.
     * 
     * @see java.io.Serializable
     */
    private static final long serialVersionUID = -7129650521543789085L;
    
    /**
     * The value contained in enum.
     */
    private final int iValue;

    /**
     * Constructor for enum item.
     *
     * @param name  the name of enum item
     * @param value  the value of enum item
     */
    protected ValuedEnum(String name, int value) {
        super(name);
        iValue = value;
    }

    /**
     * <p>Gets an <code>Enum</code> object by class and value.</p>
     *
     * <p>This method loops through the list of <code>Enum</code>,
     * thus if there are many <code>Enum</code>s this will be
     * slow.</p>
     * 
     * @param enumClass  the class of the <code>Enum</code> to get
     * @param value  the value of the <code>Enum</code> to get
     * @return the enum object, or null if the enum does not exist
     * @throws IllegalArgumentException if the enum class is <code>null</code>
     */
    protected static Enum getEnum(Class enumClass, int value) {
        if (enumClass == null) {
            throw new IllegalArgumentException("The Enum Class must not be null");
        }
        List list = Enum.getEnumList(enumClass);
        for (Iterator it = list.iterator(); it.hasNext();) {
            ValuedEnum enumeration = (ValuedEnum) it.next();
            if (enumeration.getValue() == value) {
                return enumeration;
            }
        }
        return null;
    }

    /**
     * <p>Get value of enum item.</p>
     *
     * @return the enum item's value.
     */
    public final int getValue() {
        return iValue;
    }

    /**
     * <p>Tests for order.</p>
     *
     * <p>The default ordering is numeric by value, but this
     * can be overridden by subclasses.</p>
     * 
     * @see java.lang.Comparable#compareTo(Object)
     * @param other  the other object to compare to
     * @return -ve if this is less than the other object, +ve if greater than,
     *  <code>0</code> of equal
     * @throws ClassCastException if other is not an <code>Enum</code>
     * @throws NullPointerException if other is <code>null</code>
     */
    public int compareTo(Object other) {
        return iValue - ((ValuedEnum) other).iValue;
    }

    /**
     * <p>Human readable description of this <code>Enum</code> item.</p>
     *
     * @return String in the form <code>type[name=value]</code>, for example:
     *  <code>JavaVersion[Java 1.0=100]</code>. Note that the package name is
     *  stripped from the type name.
     */
    public String toString() {
        if (iToString == null) {
            String shortName = ClassUtils.getShortClassName(getEnumClass());
            iToString = shortName + "[" + getName() + "=" + getValue() + "]";
        }
        return iToString;
    }
}
